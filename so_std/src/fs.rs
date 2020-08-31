//! Filesystem runtime functions.

use glob::glob;
use grease::{bst::BstMap, item_name, make_value, match_value, path::PathBuf, value::Value};
use serde::{Deserialize, Serialize};
use so_runtime::{script_value_as, traits, types};
use std::path::Path;

pub fn module() -> Value {
    let mut map = BstMap::new();
    map.insert("copy".into(), copy_fn());
    map.insert("exists".into(), exists_fn());
    map.insert("glob".into(), glob_fn());
    map.insert("track".into(), track_fn());
    types::Map(map).into()
}

fn glob_fn() -> Value {
    types::Function::new(|ctx| {
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

        let path = script_value_as!(
            ctx.env_get("work-dir")
                .ok_or(
                    ctx.call_site
                        .clone()
                        .with("work-dir not set")
                        .into_grease_error()
                )?
                .map(|v| v.clone())?,
            PathBuf,
            "work-dir is not a Path"
        )?;

        Ok(ctx.call_site.clone().with(make_value!((path, pattern) ["fs glob"] {
            let pattern = pattern.await?;

            let pattern = {
                let mut p = path.into_pathbuf();
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
    types::Function::new(|ctx| {
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
    })
    .into()
}

fn exists_fn() -> Value {
    types::Function::new(|ctx| {
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
    })
    .into()
}

fn track_fn() -> Value {
    types::Function::new(|ctx| {
        let path = ctx.args.next().ok_or("no file provided to track")?;

        ctx.unused_arguments()?;

        let path = path
            .map(|p| {
                match_value!(p => {
                    types::String => |v| {
                        v.get().map(|v| v.owned().to_string().into())
                    },
                    PathBuf => |v| {
                        v.get().map(|v| v.owned().into_pathbuf())
                    },
                    => |_| Err("track argument must be a string or path".into())
                })
            })
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
    })
    .into()
}

#[derive(Debug, Serialize, Deserialize)]
struct FileData {
    modification_time: std::time::SystemTime,
    content_hash: u128,
}

type TrackInfo = std::collections::HashMap<std::path::PathBuf, FileData>;
