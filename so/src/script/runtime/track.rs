//! Tracking file changes.

use super::builtin_function_prelude::*;
use grease::{item_name, make_value, match_value, Plan, TypedValue};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;

def_builtin!(ctx => {
    let path = ctx.args.next().ok_or("no file provided to track")?;

    if ctx.unused_arguments() {
        return Ok(Eval::Error);
    }

    let path = eval_error!(ctx, path
        .map(|p| {
            match_value!(p => {
                ScriptString => |v| {
                    v.get().map(|v| v.owned().into())
                },
                PathBuf => |v| {
                    v.get().map(|v| v.owned())
                },
                => |_| Err("track argument must be a string or path".into())
            })
        })
        .transpose_err());

    Config { path: path }
        .plan_split(ctx)
        .map(|v| Eval::Value(v.into()))
        .map_err(|e| e.into())
});

struct Config {
    path: PathBuf,
}

#[derive(Debug, Serialize, Deserialize)]
struct FileData {
    modification_time: std::time::SystemTime,
    content_hash: u128,
}

type TrackInfo = HashMap<PathBuf, FileData>;

impl Plan for Config {
    type Output = Result<TypedValue<PathBuf>, String>;

    fn plan(self, ctx: &mut Context) -> Self::Output {
        let store = ctx.store.item(item_name!("track"));

        // TODO inefficient to load and store every time
        let mut info = if store.exists() {
            let content = store.read().map_err(|e| e.to_string())?;
            bincode::deserialize_from(content).map_err(|e| e.to_string())?
        } else {
            TrackInfo::new()
        };

        let meta = std::fs::metadata(&self.path).map_err(|e| e.to_string())?;
        let mod_time = meta.modified().map_err(|e| e.to_string())?;

        let calc_hash = match info.get(&self.path) {
            Some(data) => data.modification_time < mod_time,
            None => true,
        };

        if calc_hash {
            let f = std::fs::File::open(&self.path).map_err(|e| e.to_string())?;
            let hash = grease::hash_read(f).map_err(|e| e.to_string())?;
            info.insert(
                self.path.clone(),
                FileData {
                    modification_time: mod_time,
                    content_hash: hash,
                },
            );
        }

        let hash = info.get(&self.path).unwrap().content_hash;

        {
            let content = store.write().map_err(|e| e.to_string())?;
            bincode::serialize_into(content, &info).map_err(|e| e.to_string())?;
        }

        Ok(make_value!((path=self.path) [hash] Ok(path)))
    }
}
