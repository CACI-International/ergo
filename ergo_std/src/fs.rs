//! Filesystem runtime functions.

use ergo_runtime::{
    context::item_name, depends, io, traits, try_result, type_system::ErgoType, types,
    value::match_value, Source, Value,
};
use glob::glob;
use serde::{Deserialize, Serialize};
use sha::sha1::Sha1;
use std::path::Path;

pub fn module() -> Value {
    crate::make_string_map! {
        "append" = append(),
        "archive" = archive(),
        "copy" = copy(),
        "create-dir" = create_dir(),
        "exists" = exists(),
        "glob" = glob_(),
        "read" = read(),
        "remove" = remove(),
        "sha1" = sha1(),
        "track" = track(),
        "unarchive" = unarchive(),
        "write" = write()
    }
}

#[types::ergo_fn]
/// Get an array of files using a glob.
///
/// Arguments: `(Into<String> :glob-pattern)`
///
/// The glob pattern is similar to unix-style globs, where:
/// * `?` matches any single character.
/// * `*` matches any (possibly empty) sequence of characters.
/// * `**` matches the current directory and arbitrary subdirectories. This
///   sequence **must** form a single path component, so both `**a` and `b**`
///   are invalid and will result in an error. A sequence of more than two
///   consecutive `*` characters is also invalid.
/// * `[...]` matches any character inside the brackets. Character sequences
///   can also specify ranges of characters, as ordered by Unicode, so e.g.
///   `[0-9]` specifies any character between 0 and 9 inclusive. An unclosed
///   bracket is invalid.
/// * `[!...]` is the negation of `[...]`, i.e. it matches any characters
///   **not** in the brackets.
/// * The metacharacters `?`, `*`, `[`, `]` can be matched by using brackets
///   (e.g. `[?]`).  When a `]` occurs immediately following `[` or `[!` then it
///   is interpreted as being part of, rather then ending, the character set, so
///   `]` and NOT `]` can be matched by `[]]` and `[!]]` respectively.  The `-`
///   character can be specified inside a character sequence pattern by placing
///   it at the start or the end, e.g. `[abc-]`.
///
/// The pattern is checked relative to the calling script directory, however specifying any path as
/// part of the pattern (paths can contain glob patterns) will result in a search using that path.
async fn glob_(pattern: _) -> Value {
    let (pattern_source, pattern) =
        try_result!(traits::into_sourced::<types::String>(CONTEXT, pattern).await).take();

    let mut path = ARGS_SOURCE
        .path()
        .unwrap_or_else(|| std::env::current_dir().unwrap());
    path.push(pattern.as_ref().as_str());

    match glob(path.to_str().unwrap()) {
        Err(e) => pattern_source.with(e).into_error().into(),
        Ok(paths) => {
            let paths: Result<Vec<std::path::PathBuf>, glob::GlobError> = paths.collect();
            let paths: Vec<Source<Value>> = try_result!(paths.map_err(|e| pattern_source.with(e)))
                .into_iter()
                .map(|v| ARGS_SOURCE.clone().with(types::Path::from(v).into()))
                .collect();
            types::Array(paths.into()).into()
        }
    }
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

#[types::ergo_fn]
/// Copy files or directories.
///
/// Arguments: `(Path :from) (Path :to)`
///
/// If the destination is an existing directory, `from` is copied with the same basename into that directory.
/// All destination directories are automatically created.
/// If `from` is a directory, it is recursively copied.
async fn copy(from: types::Path, to: types::Path) -> Value {
    let log = CONTEXT.log.sublog("fs::copy");

    let from = from.unwrap();
    let to = to.unwrap();

    log.debug(format!(
        "copying {} to {}",
        from.as_ref().0.as_ref().display(),
        to.as_ref().0.as_ref().display()
    ));

    try_result!(recursive_link(
        from.as_ref().0.as_ref(),
        to.as_ref().0.as_ref()
    ));
    types::Unit.into()
}

#[types::ergo_fn]
/// Check whether a path exists.
///
/// Arguments: `(Path :path)`
///
/// Returns a `Bool` indicating whether the path exists as a file or directory.
async fn exists(path: types::Path) -> Value {
    types::Bool(path.value().as_ref().0.as_ref().exists()).into()
}

#[types::ergo_fn]
/// Create a directory (and all ancestor directories).
///
/// Arguments: `(Path :path)`
///
/// Returns a `Unit` value that creates the directory and ancestors if they do not exist.
async fn create_dir(path: types::Path) -> Value {
    try_result!(std::fs::create_dir_all(path.value().as_ref().0.as_ref()));
    types::Unit.into()
}

#[cfg(unix)]
fn set_permissions(p: &mut std::fs::Permissions, mode: u32) {
    use std::os::unix::fs::PermissionsExt;
    p.set_mode(mode);
}

#[cfg(windows)]
fn set_permissions(p: &mut std::fs::Permissions, mode: u32) {}

#[types::ergo_fn]
/// Create an archive of the given path.
///
/// Arguments: `(Path :archive) (Path :source)`
///
/// Keyed Arguments:
/// * `String :format` - the type of archive to create. May be `dir`, `zip`, `tar`, `tar.gz`,
/// `tar.bz2` or `tar.xz`. If unspecified, the extension of `archive` is used. If there is no
/// extension, `dir` is assumed.
///
/// Note that using a `dir` archive is the same as `std:fs:copy :source :archive`.
///
/// Returns a Unit value on success.
async fn archive(archive: types::Path, source: types::Path, (format): [types::String]) -> Value {
    let (archive_source, archive) = archive.take();
    let source = source.unwrap();

    let ext: Source<String> = match format {
        Some(format) => format.map(|v| v.to_owned().0.into()),
        None => {
            let path = archive.as_ref().as_ref();
            match path.file_name() {
                None => {
                    return archive_source
                        .with("invalid archive path")
                        .into_error()
                        .into()
                }
                Some(f) => match f.to_str() {
                    None => {
                        return archive_source
                            .with("invalid unicode in filename")
                            .into_error()
                            .into()
                    }
                    Some(s) => {
                        // Skip first index in case the filename starts with a `.`, to match the
                        // behavior of std::path::Path::extension()
                        // We assume the string is not empty (otherwise file_name would have
                        // returned None).
                        let s = &s[1..];
                        archive_source.with(if let Some(ind) = s.find('.') {
                            // split_at().1 will be the extension with the leading '.', so skip it
                            s.split_at(ind).1[1..].into()
                        } else {
                            "dir".into()
                        })
                    }
                },
            }
        }
    };

    let (ext_source, ext) = ext.take();

    fn tar_to<W: std::io::Write>(w: W, dir: &Path) -> std::io::Result<W> {
        let mut builder = tar::Builder::new(w);
        builder.mode(tar::HeaderMode::Deterministic);
        builder.follow_symlinks(false);
        builder.append_dir_all(&std::path::PathBuf::default(), dir)?;
        builder.into_inner()
    }

    match ext.as_str() {
        "dir" => try_result!(recursive_link(
            source.as_ref().as_ref(),
            archive.as_ref().as_ref()
        )),
        "zip" => {
            let path = source.as_ref().as_ref();
            let f = try_result!(std::fs::File::create(archive.as_ref().as_ref())
                .map_err(|e| ARGS_SOURCE.with(e).into_error()));

            fn add_dir_entries<W: std::io::Write + std::io::Seek>(
                dir: &Path,
                prefix: &Path,
                zip: &mut zip::ZipWriter<W>,
            ) -> ergo_runtime::Result<()> {
                for entry in dir.read_dir()? {
                    let entry = entry?;
                    let name = entry.file_name();
                    let prefix_name = prefix.join(&name);
                    if entry.file_type()?.is_dir() {
                        zip.add_directory(
                            prefix_name.as_os_str().to_string_lossy(),
                            Default::default(),
                        )?;
                        add_dir_entries(&dir.join(name), &prefix_name, zip)?;
                    } else {
                        zip.start_file(
                            prefix_name.as_os_str().to_string_lossy(),
                            Default::default(),
                        )?;
                        let mut f = std::fs::File::open(dir.join(name))?;
                        std::io::copy(&mut f, zip)?;
                    }
                }
                Ok(())
            }

            let mut writer = zip::ZipWriter::new(f);
            try_result!(add_dir_entries(
                &path,
                &std::path::PathBuf::default(),
                &mut writer
            ));
            try_result!(writer.finish());
        }
        "tar" => {
            try_result!(std::fs::File::create(archive.as_ref().as_ref())
                .and_then(|p| tar_to(p, &source.as_ref().as_ref()))
                .map_err(|e| ARGS_SOURCE.with(e).into_error()));
        }
        "tar.gz" => {
            use flate2::write::GzEncoder;
            try_result!(std::fs::File::create(archive.as_ref().as_ref())
                .and_then(|p| tar_to(
                    GzEncoder::new(p, Default::default()),
                    &source.as_ref().as_ref()
                ))
                .map_err(|e| ARGS_SOURCE.with(e).into_error()));
        }
        "tar.bz2" => {
            use bzip2::write::BzEncoder;
            try_result!(std::fs::File::create(archive.as_ref().as_ref())
                .and_then(|p| tar_to(
                    BzEncoder::new(p, Default::default()),
                    &source.as_ref().as_ref()
                ))
                .map_err(|e| ARGS_SOURCE.with(e).into_error()));
        }
        "tar.xz" => {
            use xz::write::XzEncoder;
            try_result!(std::fs::File::create(archive.as_ref().as_ref())
                .and_then(|p| tar_to(
                    XzEncoder::new(p, Default::default()),
                    &source.as_ref().as_ref()
                ))
                .map_err(|e| ARGS_SOURCE.with(e).into_error()));
        }
        o => {
            return ext_source
                .with(format!("unsupported format: {}", o))
                .into_error()
                .into()
        }
    }

    types::Unit.into()
}

#[types::ergo_fn]
/// Extract an archive to a path.
///
/// Arguments: `(Path :destination) (PathOrByteStream :archive)`
///
/// `archive` may be a Path to a directory, zip file or tar archive, or a ByteStream of a zip file
/// or tar archive, where tar archives can optionally be compressed with gzip, bzip2, or lzma (xz).
/// The archive contents are extracted into `destination` as a directory.
async fn unarchive(destination: types::Path, archive: _) -> Value {
    let destination = destination.unwrap();
    let to_path = destination.as_ref().as_ref();

    let (archive_source, mut archive) = archive.take();

    try_result!(CONTEXT.eval(&mut archive).await);

    use std::io::{Read, Seek};

    fn extract_to<T: Read + Seek, P: AsRef<Path>>(
        mut archive: T,
        to_path: P,
    ) -> ergo_runtime::Result<()> {
        let to_path = to_path.as_ref();
        let mut magic = [0; 6];
        let bytes = archive.read(&mut magic)?;

        archive.seek(std::io::SeekFrom::Start(0))?;

        // zip archive
        if bytes >= 4 && magic[0..4] == [b'P', b'K', 3, 4] {
            use zip::ZipArchive;
            let mut archive = ZipArchive::new(archive)?;
            for i in 0..archive.len() {
                let mut file = archive.by_index(i)?;
                if file.is_dir() {
                    std::fs::create_dir_all(to_path.join(file.name()))?;
                } else if file.is_file() {
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
            // TODO check whether file is a tar archive (need to buffer and seek into output
            // stream)

            use tar::Archive;
            // gzip
            if bytes >= 2 && magic[0..2] == [0x1f, 0x8b] {
                use flate2::read::GzDecoder;
                Archive::new(GzDecoder::new(archive)).unpack(to_path)?;
            }
            // bzip2
            else if bytes >= 3 && magic[0..3] == [b'B', b'Z', b'h'] {
                use bzip2::read::BzDecoder;
                Archive::new(BzDecoder::new(archive)).unpack(to_path)?;
            }
            // lzma
            else if bytes >= 6 && magic[0..6] == [0xfd, b'7', b'z', b'X', b'Z', 0] {
                use xz::read::XzDecoder;
                Archive::new(XzDecoder::new(archive)).unpack(to_path)?;
            } else {
                Archive::new(archive).unpack(to_path)?;
            };
        }

        Ok(())
    }

    match_value! { archive,
        types::Path(p) => {
            let path = p.as_ref();
            if path.is_dir() {
                try_result!(recursive_link(path, to_path));
            } else if path.is_file() {
                try_result!(extract_to(try_result!(std::fs::File::open(path)), to_path).map_err(|e| ARGS_SOURCE.with(e.error())));
            }
            else {
                return archive_source
                    .with("path is not a file nor directory")
                    .into_error()
                    .into();
            }
        }
        bs@types::ByteStream {..} => {
            // Read the entire byte stream into memory (no AsyncRead support in decoders)
            let mut data = Vec::new();
            use io::AsyncReadExt;
            try_result!(bs.read().read_to_end(&CONTEXT.task, &mut data).await);
            try_result!(extract_to(std::io::Cursor::new(data), to_path));
        }
        v => return traits::type_error(CONTEXT, archive_source.with(v), "Path or ByteStream").into()
    }

    types::Unit.into()
}

#[types::ergo_fn]
/// Check the sha1sum of a file.
///
/// Arguments: `(Path :file) (String :sum)`
///
/// Returns a boolean indicating whether `sum` is the sha1 sum of the contents of `file`.
async fn sha1(file: types::Path, sum: types::String) -> Value {
    let mut f = try_result!(std::fs::File::open(file.value().as_ref().as_ref()));
    let mut digest = Sha1::default();
    try_result!(std::io::copy(&mut f, &mut digest));
    use sha::utils::DigestExt;
    types::Bool(
        digest
            .to_hex()
            .eq_ignore_ascii_case(sum.value().as_ref().as_str()),
    )
    .into()
}

#[types::ergo_fn]
/// Make a Path depend on the contents of the file to which it refers.
///
/// Arguments: `(StringOrPath :file)`
///
/// Returns a `Path` that is identified by the contents of the file to which the argument refers.
/// `file` must exist and refer to a file.
async fn track(file: _) -> Value {
    let (file_source, file) = file.take();
    let file = match_value! {file,
        types::String(s) => s.as_str().into(),
        types::Path(p) => p.into_pathbuf(),
        v => return traits::type_error(CONTEXT, file_source.with(v), "String or Path").into()
    };

    let store = CONTEXT.store.item(item_name!("track"));

    let loaded_info = try_result!(CONTEXT
        .shared_state
        .get(move || LoadedTrackInfo::new(store)));
    let guard = try_result!(loaded_info.info.read().map_err(|_| "poisoned"));

    let meta = try_result!(std::fs::metadata(&file));
    let mod_time = try_result!(meta.modified());

    let calc_hash = guard
        .get(&file)
        .map(|data| data.modification_time < mod_time)
        .unwrap_or(true);

    let hash = if calc_hash {
        let f = try_result!(std::fs::File::open(&file));
        let hash = try_result!(ergo_runtime::hash::hash_read(f));
        drop(guard);
        let mut guard = try_result!(loaded_info.info.write().map_err(|_| "poisoned"));
        guard.insert(
            file.clone(),
            FileData {
                modification_time: mod_time,
                content_hash: hash,
            },
        );
        hash
    } else {
        let hash = guard.get(&file).unwrap().content_hash;
        drop(guard);
        hash
    };

    let mut v: Value = types::Path::from(file).into();
    v.set_dependencies(depends![v.id(), hash]);
    v
}

#[derive(Debug, Serialize, Deserialize)]
struct FileData {
    modification_time: std::time::SystemTime,
    content_hash: u128,
}

type TrackInfo = std::collections::HashMap<std::path::PathBuf, FileData>;

#[derive(ErgoType)]
struct LoadedTrackInfo {
    pub info: std::sync::RwLock<TrackInfo>,
    item: ergo_runtime::context::Item,
}

impl LoadedTrackInfo {
    pub fn new(item: ergo_runtime::context::Item) -> ergo_runtime::Result<Self> {
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

#[types::ergo_fn]
/// Remove a file or directory (recursively).
///
/// Arguments: `(Path :path)`
///
/// If the path does not exist, nothing happens.
async fn remove(path: types::Path) -> Value {
    let path = path.value().as_ref().as_ref();
    if path.is_file() {
        try_result!(std::fs::remove_file(path));
    } else if path.is_dir() {
        try_result!(std::fs::remove_dir_all(path));
    }
    types::Unit.into()
}

#[types::ergo_fn]
/// Read a file as a ByteStream.
///
/// Arguments: `(Path :file)`
///
/// Returns a `ByteStream` of the file's contents.
async fn read(file: types::Path) -> Value {
    let path = file.value().as_ref().as_ref();
    let hash = try_result!(ergo_runtime::hash::hash_read(try_result!(
        std::fs::File::open(&path)
    )));
    Value::constant_deps(
        types::ByteStream::new(io::Blocking::new(try_result!(std::fs::File::open(path)))),
        depends![hash],
    )
}

#[types::ergo_fn]
/// Write a ByteStream to a file.
///
/// Arguments: `(Path :file) (Into<ByteStream> :bytes)`
///
/// Creates or overwrites the file with the bytes from the ByteStream.
async fn write(file: types::Path, bytes: _) -> Value {
    let file = file.unwrap();
    let bytes =
        try_result!(traits::into_sourced::<types::ByteStream>(CONTEXT, bytes).await).unwrap();

    let mut f = io::Blocking::new(try_result!(std::fs::File::create(file.as_ref().as_ref())));
    try_result!(ergo_runtime::io::copy(&CONTEXT.task, &mut bytes.as_ref().read(), &mut f).await);
    types::Unit.into()
}

#[types::ergo_fn]
/// Append a ByteStream to a file.
///
/// Arguments: `(Path :file) (Into<ByteStream> :bytes)`
/// Creates or appends the file with the bytes from the ByteStream.
async fn append(file: types::Path, bytes: _) -> Value {
    let file = file.unwrap();
    let bytes =
        try_result!(traits::into_sourced::<types::ByteStream>(CONTEXT, bytes).await).unwrap();

    let mut f = io::Blocking::new(try_result!(std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(file.as_ref().as_ref())));
    try_result!(ergo_runtime::io::copy(&CONTEXT.task, &mut bytes.as_ref().read(), &mut f).await);
    types::Unit.into()
}
