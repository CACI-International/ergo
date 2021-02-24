//! Filesystem runtime functions.

use ergo_runtime::{ergo_function, types, ContextExt};
use glob::glob;
use grease::{
    item_name, make_value, match_value, path::PathBuf, runtime::io::Blocking, types::GreaseType,
    value::Value,
};
use serde::{Deserialize, Serialize};
use sha::sha1::Sha1;
use std::path::Path;

pub fn module() -> Value {
    crate::grease_string_map! {
        "append" = append_fn(),
        "copy" = copy_fn(),
        "create-dir" = create_dir_fn(),
        "exists" = exists_fn(),
        "glob" = glob_fn(),
        "read" = read_fn(),
        "remove" = remove_fn(),
        "sha1" = sha1_fn(),
        "track" = track_fn(),
        "unarchive" = unarchive_fn(),
        "write" = write_fn()
    }
}

fn glob_fn() -> Value {
    ergo_function!(std::fs::glob,
    r"Get an array of files using a glob.

Arguments: <glob-pattern: Into<String>>
The glob pattern is similar to unix-style globs, where:
* `?` matches any single character.
* `*` matches any (possibly empty) sequence of characters.
* `**` matches the current directory and arbitrary subdirectories. This
  sequence **must** form a single path component, so both `**a` and `b**`
  are invalid and will result in an error.  A sequence of more than two
  consecutive `*` characters is also invalid.
* `[...]` matches any character inside the brackets.  Character sequences
  can also specify ranges of characters, as ordered by Unicode, so e.g.
  `[0-9]` specifies any character between 0 and 9 inclusive. An unclosed
  bracket is invalid.
* `[!...]` is the negation of `[...]`, i.e. it matches any characters
  **not** in the brackets.
* The metacharacters `?`, `*`, `[`, `]` can be matched by using brackets
  (e.g. `[?]`).  When a `]` occurs immediately following `[` or `[!` then it
  is interpreted as being part of, rather then ending, the character set, so
  `]` and NOT `]` can be matched by `[]]` and `[!]]` respectively.  The `-`
  character can be specified inside a character sequence pattern by placing
  it at the start or the end, e.g. `[abc-]`.

The pattern is checked relative to the script directory, however specifying any path as part of the pattern (paths can
have glob patterns) will result in a search using that path.",
    |ctx,args| {
        let pattern = args.next().ok_or("no glob pattern provided")?;

        args.unused_arguments()?;

        let pattern_source = pattern.source();
        let pattern = ctx.into_sourced::<types::String>(pattern);
        let pattern = pattern.await?.unwrap();

        let path = ctx.mod_dir();

        make_value!((path, pattern) {
            let pattern = pattern.await?;

            let pattern = {
                let mut p = path;
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
        .into()
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
        if let Some(parent) = to.as_ref().parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::hard_link(from, to)
    }
}

fn copy_fn() -> Value {
    ergo_function!(std::fs::copy,
    r"Copy files or directories.

Arguments: <from: Path> <to: Path>
If the destination is an existing directory, `from` is copied with the same basename into that directory.
All destination directories are automatically created.
If `from` is a directory, it is recursively copied.",
    |ctx,args| {
        let from = args.next().ok_or("'from' missing")?;
        let to = args.next().ok_or("'to' missing")?;

        args.unused_arguments()?;

        let from = ctx.source_value_as::<PathBuf>(from);
        let from = from.await?.unwrap();

        let to = ctx.source_value_as::<PathBuf>(to);
        let to = to.await?.unwrap();

        let log = ctx.log.sublog("fs::copy");
        let task = ctx.task.clone();
        make_value!((from,to) {
            let (from,to) = task.join(from, to).await?;

            log.debug(format!("copying {} to {}", from.as_ref().as_ref().display(), to.as_ref().as_ref().display()));
            recursive_link(from.as_ref().as_ref(), to.as_ref().as_ref())?;
            Ok(types::Unit)
        })
        .into()
    })
    .into()
}

fn exists_fn() -> Value {
    ergo_function!(
        std::fs::exists,
        r"Check whether a path exists.

Arguments: <Path>
Returns a boolean indicating whether the path exists (as either a file or directory).",
        |ctx, args| {
            let path = args.next().ok_or("'path' missing")?;

            args.unused_arguments()?;

            let path = ctx.source_value_as::<PathBuf>(path);
            let path = path.await?.unwrap();

            make_value!((path) {
                Ok(types::Bool(path.await?.as_ref().as_ref().exists()))
            })
            .into()
        }
    )
    .into()
}

fn create_dir_fn() -> Value {
    ergo_function!(
        std::fs::create_dir,
        r"Create a directory (and all ancestor directories).

Arguments: <Path>
Returns a unit-typed value that creates the directory and ancestors if they do not exist.",
        |ctx, args| {
            let path = args.next().ok_or("'path' missing")?;

            args.unused_arguments()?;

            let path = ctx.source_value_as::<PathBuf>(path);
            let path = path.await?.unwrap();

            make_value!([path] {
                std::fs::create_dir_all(path.await?.as_ref().as_ref())?;
                Ok(types::Unit)
            })
            .into()
        }
    )
    .into()
}

#[cfg(unix)]
fn set_permissions(p: &mut std::fs::Permissions, mode: u32) {
    use std::os::unix::fs::PermissionsExt;
    p.set_mode(mode);
}

#[cfg(windows)]
fn set_permissions(p: &mut std::fs::Permissions, mode: u32) {}

fn unarchive_fn() -> Value {
    ergo_function!(std::fs::unarchive,
    r"Extract an archive to a path.

Arguments: <archive: Path> <destination: Path>
`archive` may be a path to a directory, zip file or tar archive, where the tar archive can optionally be compressed with
gzip, bzip2, or lzma (xz). The archive contents are extracted into `destination` as a directory.",
    |ctx,args| {
        let from = args.next().ok_or("'from' missing")?;
        let to = args.next().ok_or("'to' missing")?;

        args.unused_arguments()?;

        let from = ctx.source_value_as::<PathBuf>(from);
        let (from_source, from) = from.await?.take();
        let to = ctx.source_value_as::<PathBuf>(to);
        let to = to.await?.unwrap();

        let task = ctx.task.clone();
        make_value!([from, to] {
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
            Ok(types::Unit)
        }).into()
    }).into()
}

fn sha1_fn() -> Value {
    ergo_function!(
        std::fs::sha1,
        r"Check the sha1sum of a file.

Arguments: <file: Path> <sum: String>
Returns a boolean indicating whether `sum` is the sha1 sum of the contents of `file`.",
        |ctx, args| {
            let path = args.next().ok_or("no file provided to sha1")?;
            let sum = args.next().ok_or("no checksum provided")?;

            args.unused_arguments()?;

            let path = ctx.source_value_as::<PathBuf>(path);
            let path = path.await?.unwrap();
            let sum = ctx.source_value_as::<types::String>(sum);
            let sum = sum.await?.unwrap();

            let task = ctx.task.clone();
            make_value!([path, sum] {
                let (path,sum) = task.join(path, sum).await?;

                let mut f = std::fs::File::open(path.as_ref().as_ref())?;
                let mut digest = Sha1::default();
                std::io::copy(&mut f, &mut digest)?;
                use sha::utils::DigestExt;
                Ok(types::Bool(digest.to_hex().eq_ignore_ascii_case(sum.as_ref().as_str())))
            })
            .into()
        }
    )
    .into()
}

fn track_fn() -> Value {
    ergo_function!(independent std::fs::track,
    r"Make a Path depend on the contents of the file to which it refers.

Arguments: <Path-or-String>
Returns a Path that is identified by the contents of the file to which the argument refers. The path must exist and
refer to a file.",
    |ctx,args| {
        let path = args.next().ok_or("no file provided to track")?;

        args.unused_arguments()?;

        let path = path
            .map_async(|p|
                match_value!(p => {
                    types::String => |v| {
                        v.await?.owned().to_string().into()
                    },
                    PathBuf => |v| {
                        v.await?.owned().into_pathbuf()
                    },
                    => |_| Err("track argument must be a string or path")?
                })
            )
            .await
            .transpose_err()
            .map_err(|e| e.into_grease_error())?;

        let store = ctx.store.item(item_name!("track"));

        let loaded_info = ctx.shared_state(move || LoadedTrackInfo::new(store))?;
        let guard = loaded_info.info.read().map_err(|_| "poisoned")?;

        let meta = std::fs::metadata(&path)?;
        let mod_time = meta.modified()?;

        let calc_hash = match guard.get(&path) {
            Some(data) => data.modification_time < mod_time,
            None => true,
        };

        let hash = if calc_hash {
            let f = std::fs::File::open(&path)?;
            let hash = grease::hash::hash_read(f)?;
            drop(guard);
            let mut guard = loaded_info.info.write().map_err(|_| "poisoned")?;
            guard.insert(
                path.clone(),
                FileData {
                    modification_time: mod_time,
                    content_hash: hash,
                },
            );
            hash
        } else {
            let hash = guard.get(&path).unwrap().content_hash;
            drop(guard);
            hash
        };

        make_value!((path) [hash] Ok(PathBuf::from(path))).into()
    })
    .into()
}

#[derive(Debug, Serialize, Deserialize)]
struct FileData {
    modification_time: std::time::SystemTime,
    content_hash: u128,
}

type TrackInfo = std::collections::HashMap<std::path::PathBuf, FileData>;

#[derive(GreaseType)]
struct LoadedTrackInfo {
    pub info: std::sync::RwLock<TrackInfo>,
    item: grease::runtime::Item,
}

impl LoadedTrackInfo {
    pub fn new(item: grease::runtime::Item) -> grease::Result<Self> {
        let info = if item.exists() {
            let content = item.read()?;
            bincode::deserialize_from(content).map_err(|e| e.to_string())?
        } else {
            TrackInfo::new()
        };
        Ok(LoadedTrackInfo {
            info: std::sync::RwLock::new(info),
            item,
        })
    }
}

impl std::ops::Drop for LoadedTrackInfo {
    fn drop(&mut self) {
        if let Ok(info) = std::mem::take(&mut self.info).into_inner() {
            let content = match self.item.write() {
                Err(e) => {
                    eprintln!("could not open fs::track info for writing: {}", e);
                    return;
                }
                Ok(v) => v,
            };
            if let Err(e) = bincode::serialize_into(content, &info) {
                eprintln!("error while serializing fs::track info: {}", e);
            }
        } else {
            eprintln!("poisoned lock guard in fs::track info; not storing");
        }
    }
}

fn remove_fn() -> Value {
    ergo_function!(
        std::fs::remove,
        r"Remove a file or directory (recursively).

Arguments: <Path>
If the path does not exist, nothing happens.",
        |ctx, args| {
            let path = args.next().ok_or("'path' missing")?;

            args.unused_arguments()?;

            let path = ctx.source_value_as::<PathBuf>(path);
            let path = path.await?.unwrap();

            make_value!([path] {
                let path = path.await?;
                let path = path.as_ref().as_ref();
                if path.is_file() {
                    std::fs::remove_file(path)?;
                }
                else if path.is_dir() {
                    std::fs::remove_dir_all(path)?;
                }
                Ok(types::Unit)
            })
            .into()
        }
    )
    .into()
}

fn read_fn() -> Value {
    ergo_function!(
        std::fs::read,
        r"Read a file as a ByteStream.

Arguments: <Path>
Returns a ByteStream of the file's contents.",
        |ctx, args| {
            let path = args.next().ok_or("'path' missing")?;

            args.unused_arguments()?;

            let path = ctx.source_value_as::<PathBuf>(path);
            let path = path.await?.unwrap();

            make_value!([path] {
                let path = path.await?;
                let path = path.as_ref().as_ref();
                Ok(types::ByteStream::new(Blocking::new(std::fs::File::open(path)?)))
            })
            .into()
        }
    )
    .into()
}

fn write_fn() -> Value {
    ergo_function!(
        std::fs::write,
        r"Write a ByteStream to a file.

Arguments: <Path> <ByteStream>
Creates or overwrites the file with the bytes from the ByteStream.",
        |ctx, args| {
            let path = args.next().ok_or("'path' missing")?;
            let data = args.next().ok_or("'data' missing")?;

            args.unused_arguments()?;

            let path = ctx.source_value_as::<PathBuf>(path);
            let path = path.await?.unwrap();

            let data = ctx.into_sourced::<types::ByteStream>(data);
            let data = data.await?.unwrap();

            let task = ctx.task.clone();

            make_value!([path,data] {
                let (path, data) = task.join(path,data).await?;
                let mut f = Blocking::new(std::fs::File::create(path.as_ref().as_ref())?);
                grease::runtime::io::copy(&task, &mut data.read(), &mut f).await?;
                Ok(types::Unit)
            })
            .into()
        }
    )
    .into()
}

fn append_fn() -> Value {
    ergo_function!(
        std::fs::append,
        r"Append a ByteStream to a file.

Arguments: <Path> <ByteStream>
Creates or appends the file with the bytes from the ByteStream.",
        |ctx, args| {
            let path = args.next().ok_or("'path' missing")?;
            let data = args.next().ok_or("'data' missing")?;

            args.unused_arguments()?;

            let path = ctx.source_value_as::<PathBuf>(path);
            let path = path.await?.unwrap();

            let data = ctx.into_sourced::<types::ByteStream>(data);
            let data = data.await?.unwrap();

            let task = ctx.task.clone();

            make_value!([path,data] {
                let (path, data) = task.join(path,data).await?;
                let mut f = Blocking::new(std::fs::OpenOptions::new().create(true).append(true).open(path.as_ref().as_ref())?);
                grease::runtime::io::copy(&task, &mut data.read(), &mut f).await?;
                Ok(types::Unit)
            })
            .into()
        }
    )
    .into()
}
