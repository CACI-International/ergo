//! Filesystem runtime functions.

use ergo_runtime::{source_value_as, traits, types};
use futures::future::FutureExt;
use glob::glob;
use grease::{bst::BstMap, item_name, make_value, match_value, path::PathBuf, value::Value};
use serde::{Deserialize, Serialize};
use sha::sha1::Sha1;
use std::path::Path;

pub fn module() -> Value {
    let mut map = BstMap::new();
    map.insert("copy".into(), copy_fn());
    map.insert("exists".into(), exists_fn());
    map.insert("glob".into(), glob_fn());
    map.insert("mount".into(), mount_fn());
    map.insert("sha1".into(), sha1_fn());
    map.insert("track".into(), track_fn());
    types::Map(map).into()
}

fn glob_fn() -> Value {
    types::Function::new(|ctx| {
        async move {
            let pattern = ctx.args.next().ok_or("no glob pattern provided")?;

            ctx.unused_arguments()?;

            let pattern_source = pattern.source();
            let pattern = pattern
                .map(|v| {
                    ctx.traits
                        .get::<traits::IntoTyped<types::String>>(&v)
                        .ok_or("cannot convert glob pattern value into string")
                        .map(|t| t.into_typed(v))
                })
                .transpose_err()
                .map_err(|e| e.into_grease_error())?;

            let path = source_value_as!(
                ctx.env_get("work-dir")
                    .ok_or(
                        ctx.call_site
                            .clone()
                            .with("work-dir not set")
                            .into_grease_error()
                    )?
                    .map(|v| v.clone())?,
                PathBuf,
                ctx
            )?
            .unwrap();

            let task = ctx.task.clone();
            Ok(ctx.call_site.clone().with(make_value!((path, pattern) ["fs glob"] {
            let (path, pattern) = task.join(path,pattern).await?;

            let pattern = {
                let mut p = path.owned().into_pathbuf();
                p.push(pattern.as_str());
                p
            };

            match glob(pattern.to_str().unwrap()) {
                Err(e) => Err(pattern_source.with(e).into()),
                Ok(paths) => {
                    let paths: Result<Vec<std::path::PathBuf>, glob::GlobError> = paths.collect();
                    let paths: Vec<Value> = paths
                        .map_err(|e| pattern_source.with(e))?
                        .into_iter()
                        .map(|v| PathBuf::from(v).into())
                        .collect();
                    Ok(types::Array(paths.into()))
                }
            }
        })
        .into()))
        }
        .boxed()
    })
    .into()
}

fn recursive_link<F: AsRef<Path>, T: AsRef<Path>>(from: F, to: T) -> Result<(), std::io::Error> {
    if to.as_ref().is_dir() {
        let mut to = to.as_ref().to_owned();
        to.push(from.as_ref().file_name().expect("path ends in .."));
        return recursive_link(from, &to);
    }

    if to.as_ref().is_file() {
        std::fs::remove_file(to.as_ref())?;
    }

    let meta = std::fs::metadata(from.as_ref())?;
    if meta.is_dir() {
        std::fs::create_dir_all(to.as_ref())?;
        for d in from.as_ref().read_dir()? {
            let d = d?;
            let name = d.file_name();
            let mut to = to.as_ref().to_owned();
            to.push(name);
            recursive_link(d.path(), &to)?;
        }
        Ok(())
    } else {
        std::fs::hard_link(from, to)
    }
}

fn copy_fn() -> Value {
    types::Function::new(|ctx| async move {
        let from = ctx.args.next().ok_or("'from' missing")?;
        let to = ctx.args.next().ok_or("'to' missing")?;

        ctx.unused_arguments()?;

        let from = from.map(|v| v.typed::<PathBuf>()
            .map_err(|_| "'from' argument must be a path"))
            .transpose_err()
            .map_err(|e| e.into_grease_error())?;

        let to = to.map(|v| v.typed::<PathBuf>()
            .map_err(|_| "'to' argument must be a path"))
            .transpose_err()
            .map_err(|e| e.into_grease_error())?;

        let log = ctx.log.sublog("fs::copy");
        let task = ctx.task.clone();
        Ok(ctx.call_site.clone().with(make_value!((from,to) ["fs copy"] {
            let (from,to) = task.join(from, to).await?;

            log.debug(format!("copying {} to {}", from.as_ref().as_ref().display(), to.as_ref().as_ref().display()));

            Ok(recursive_link(from.as_ref().as_ref(), to.as_ref().as_ref())?)
        })
        .into()))
    }.boxed())
    .into()
}

fn exists_fn() -> Value {
    types::Function::new(|ctx| {
        async move {
            let path = ctx.args.next().ok_or("'path' missing")?;

            ctx.unused_arguments()?;

            let path = path
                .map(|v| {
                    v.typed::<PathBuf>()
                        .map_err(|_| "'path' argument must be a path")
                })
                .transpose_err()
                .map_err(|e| e.into_grease_error())?;

            Ok(ctx.call_site.clone().with(
                make_value!((path) ["fs exists"] {
                    Ok(path.await?.as_ref().as_ref().exists())
                })
                .into(),
            ))
        }
        .boxed()
    })
    .into()
}

#[cfg(unix)]
fn set_permissions(p: &mut std::fs::Permissions, mode: u32) {
    use std::os::unix::fs::PermissionsExt;
    p.set_mode(mode);
}

#[cfg(windows)]
fn set_permissions(p: &mut std::fs::Permissions, mode: u32) {}

fn mount_fn() -> Value {
    types::Function::new(|ctx| {
        async move {
            let from = ctx.args.next().ok_or("mount 'from' missing")?;
            let to = ctx.args.next().ok_or("mount 'to' missing")?;

            ctx.unused_arguments()?;

            let (from_source, from) = source_value_as!(from, PathBuf, ctx)?.take();
            let to = source_value_as!(to, PathBuf, ctx)?.unwrap();

            let task = ctx.task.clone();
            Ok(ctx.call_site.clone().with(make_value!(["fs mount", from, to] {
                let (from, to) = task.join(from, to).await?;

                let from_path = from.as_ref().as_ref();
                let to_path = to.as_ref().as_ref();
                if from_path.is_dir() {
                    recursive_link(from_path, to_path)?;
                }
                else if from_path.is_file() {
                    let mut f = std::fs::File::open(from_path)?;
                    use std::io::Read;
                    use std::io::Seek;

                    let mut magic = [0; 6];
                    if let Ok(bytes) = f.read(&mut magic) {
                        f.seek(std::io::SeekFrom::Start(0))?;

                        // zip archive
                        if bytes >= 4 && magic[0..4] == [b'P', b'K', 3, 4] {
                            use zip::ZipArchive;
                            let mut archive = ZipArchive::new(f)?;
                            for i in 0..archive.len() {
                                let mut file = archive.by_index(i)?;
                                if file.is_dir() {
                                    std::fs::create_dir_all(to_path.join(file.name()))?;
                                }
                                else if file.is_file() {
                                    let p = to_path.join(file.name());
                                    std::fs::create_dir_all(p.parent().expect("no parent path in zip output"))?;
                                    let mut to_file = std::fs::File::create(p)?;
                                    std::io::copy(&mut file, &mut to_file)?;
                                    let mut permissions = to_file.metadata()?.permissions();
                                    if let Some(mode) = file.unix_mode() {
                                        set_permissions(&mut permissions, mode);
                                    }
                                    to_file.set_permissions(permissions)?;
                                }
                            }
                        } else {
                            let archive : Box<dyn std::io::Read> =
                                // gzip
                                if bytes >= 2 && magic[0..2] == [0x1f, 0x8b] {
                                    use flate2::read::GzDecoder;
                                    Box::new(GzDecoder::new(f))
                                }
                                // bzip2
                                else if bytes >= 3 && magic[0..3] == [b'B', b'Z', b'h'] {
                                    use bzip2::read::BzDecoder;
                                    Box::new(BzDecoder::new(f))
                                }
                                // lzma
                                else if bytes >= 6 && magic[0..6] == [0xfd, b'7', b'z', b'X', b'Z', 0] {
                                    use xz::read::XzDecoder;
                                    Box::new(XzDecoder::new(f))
                                } else {
                                    Box::new(f)
                                };

                            use tar::Archive;
                            // TODO check whether file is a tar archive (need to buffer and seek into output
                            // stream)
                            let mut tar = Archive::new(archive);
                            tar.unpack(to_path)?;
                        }
                    }
                }
                else {
                    return Err(from_source.with("path is not a file nor directory").into_grease_error());
                }
                Ok(())
            }).into()))
        }.boxed()
    }).into()
}

fn sha1_fn() -> Value {
    types::Function::new(|ctx| {
        async move {
            let path = ctx.args.next().ok_or("no file provided to sha1")?;
            let sum = ctx.args.next().ok_or("no checksum provided")?;

            ctx.unused_arguments()?;

            let path = source_value_as!(path, PathBuf, ctx)?.unwrap();
            let sum = source_value_as!(sum, types::String, ctx)?.unwrap();

            let task = ctx.task.clone();
            Ok(ctx.call_site.clone().with(
                make_value!(["fs sha1", path, sum] {
                    let (path,sum) = task.join(path, sum).await?;

                    let mut f = std::fs::File::open(path.as_ref().as_ref())?;
                    let mut digest = Sha1::default();
                    std::io::copy(&mut f, &mut digest)?;
                    use sha::utils::DigestExt;
                    Ok(digest.to_hex().eq_ignore_ascii_case(sum.as_ref().as_str()))
                })
                .into(),
            ))
        }
        .boxed()
    })
    .into()
}

fn track_fn() -> Value {
    types::Function::new(|ctx| {
        async move {
            let path = ctx.args.next().ok_or("no file provided to track")?;

            ctx.unused_arguments()?;

            let path = path
                .map_async(|p| async {
                    match_value!(p => {
                        types::String => |v| {
                            v.await.map(|v| v.owned().to_string().into())
                        },
                        PathBuf => |v| {
                            v.await.map(|v| v.owned().into_pathbuf())
                        },
                        => |_| Err("track argument must be a string or path".into())
                    })
                })
                .await
                .transpose_err()
                .map_err(|e| e.into_grease_error())?;

            let store = ctx.store.item(item_name!("track"));

            // TODO inefficient to load and store every time
            let mut info = if store.exists() {
                let content = store.read()?;
                bincode::deserialize_from(content).map_err(|e| e.to_string())?
            } else {
                TrackInfo::new()
            };

            let meta = std::fs::metadata(&path)?;
            let mod_time = meta.modified()?;

            let calc_hash = match info.get(&path) {
                Some(data) => data.modification_time < mod_time,
                None => true,
            };

            if calc_hash {
                let f = std::fs::File::open(&path)?;
                let hash = grease::hash::hash_read(f)?;
                info.insert(
                    path.clone(),
                    FileData {
                        modification_time: mod_time,
                        content_hash: hash,
                    },
                );
            }

            let hash = info.get(&path).unwrap().content_hash;

            {
                let content = store.write()?;
                bincode::serialize_into(content, &info).map_err(|e| e.to_string())?;
            }

            Ok(ctx
                .call_site
                .clone()
                .with(make_value!((path) [hash] Ok(PathBuf::from(path))).into()))
        }
        .boxed()
    })
    .into()
}

#[derive(Debug, Serialize, Deserialize)]
struct FileData {
    modification_time: std::time::SystemTime,
    content_hash: u128,
}

type TrackInfo = std::collections::HashMap<std::path::PathBuf, FileData>;
