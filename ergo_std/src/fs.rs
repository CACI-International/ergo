//! Filesystem runtime functions.

use ergo_runtime::{
    context::item_name,
    depends,
    io::{self, AddContext},
    metadata::Source,
    traits, try_result,
    type_system::ErgoType,
    types,
    value::match_value,
    Context, Value,
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
    let pattern = try_result!(traits::into::<types::String>(pattern).await);
    let pattern_source = Source::get(&pattern);

    let mut path = ARGS_SOURCE
        .path()
        .map(|p| p.parent().unwrap().into())
        .unwrap_or_else(|| std::env::current_dir().unwrap());
    path.push(pattern.as_ref().as_str());

    match glob(path.to_str().unwrap()) {
        Err(e) => pattern_source.with(e).into_error().into(),
        Ok(paths) => {
            let paths: Result<Vec<std::path::PathBuf>, glob::GlobError> = paths.collect();
            let paths: Vec<Value> = try_result!(paths.map_err(|e| pattern_source.with(e)))
                .into_iter()
                .map(|v| Source::imbue(ARGS_SOURCE.clone().with(types::Path::from(v).into())))
                .collect();
            types::Array(paths.into()).into()
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum LinkMode {
    Copy,
    Hard,
    Symbolic,
    Fallback(Vec<LinkMode>),
}

impl std::str::FromStr for LinkMode {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "symbolic" => Ok(LinkMode::Symbolic),
            "hard" => Ok(LinkMode::Hard),
            "none" => Ok(LinkMode::Copy),
            _ => Err("unsupported shallow specification"),
        }
    }
}

#[cfg(unix)]
fn symlink_dir<P: AsRef<Path>, Q: AsRef<Path>>(original: P, link: Q) -> std::io::Result<()> {
    std::os::unix::fs::symlink(original, link)
}

#[cfg(windows)]
fn symlink_dir<P: AsRef<Path>, Q: AsRef<Path>>(original: P, link: Q) -> std::io::Result<()> {
    std::os::windows::fs::symlink_dir(original, link)
}

#[cfg(unix)]
fn symlink_file<P: AsRef<Path>, Q: AsRef<Path>>(original: P, link: Q) -> std::io::Result<()> {
    std::os::unix::fs::symlink(original, link)
}

#[cfg(windows)]
fn symlink_file<P: AsRef<Path>, Q: AsRef<Path>>(original: P, link: Q) -> std::io::Result<()> {
    std::os::windows::fs::symlink_file(original, link)
}

fn recursive_link<F: AsRef<Path>, T: AsRef<Path>>(
    from: F,
    to: T,
    mode: &LinkMode,
) -> std::io::Result<()> {
    if to.as_ref().is_dir() {
        let mut to = to.as_ref().to_owned();
        to.push(from.as_ref().file_name().expect("path ends in .."));
        return recursive_link(from, &to, mode);
    }

    if to.as_ref().is_file() {
        std::fs::remove_file(to.as_ref()).add_context_str(to.as_ref().display())?;
    }

    let meta = std::fs::metadata(from.as_ref()).add_context_str(from.as_ref().display())?;
    if meta.is_dir() {
        if mode == &LinkMode::Symbolic {
            symlink_dir(from.as_ref(), to.as_ref()).add_context_with(|| {
                format!(
                    "while symlinking {} to {}",
                    from.as_ref().display(),
                    to.as_ref().display()
                )
            })?;
        } else {
            std::fs::create_dir_all(to.as_ref()).add_context_str(to.as_ref().display())?;
            for d in from
                .as_ref()
                .read_dir()
                .add_context_str(from.as_ref().display())?
            {
                let d = d.add_context_str(from.as_ref().display())?;
                let name = d.file_name();
                let mut to = to.as_ref().to_owned();
                to.push(name);
                recursive_link(d.path(), &to, mode)?;
            }
        }
        Ok(())
    } else {
        if let Some(parent) = to.as_ref().parent() {
            std::fs::create_dir_all(parent).add_context_str(parent.display())?;
        }

        fn do_copy<P: AsRef<Path>, Q: AsRef<Path>>(
            from: P,
            to: Q,
            mode: &LinkMode,
        ) -> std::io::Result<()> {
            match mode {
                LinkMode::Copy => std::fs::copy(from.as_ref(), to.as_ref())
                    .add_context_with(|| {
                        format!(
                            "while copying {} to {}",
                            from.as_ref().display(),
                            to.as_ref().display()
                        )
                    })
                    .map(|_| ()),
                LinkMode::Hard => {
                    std::fs::hard_link(from.as_ref(), to.as_ref()).add_context_with(|| {
                        format!(
                            "while hardlinking {} to {}",
                            from.as_ref().display(),
                            to.as_ref().display()
                        )
                    })
                }
                LinkMode::Symbolic => {
                    symlink_file(from.as_ref(), to.as_ref()).add_context_with(|| {
                        format!(
                            "while symlinking {} to {}",
                            from.as_ref().display(),
                            to.as_ref().display()
                        )
                    })
                }
                LinkMode::Fallback(modes) => {
                    let mut last_result = Ok(());
                    for m in modes {
                        last_result = do_copy(from.as_ref(), to.as_ref(), m);
                        if last_result.is_ok() {
                            return Ok(());
                        }
                    }
                    last_result
                }
            }
        }

        do_copy(from, to, mode)
    }
}

#[types::ergo_fn]
/// Copy files or directories.
///
/// Arguments: `(Path :from) (Path :to)`
///
/// Keyed Arguments:
/// * `:shallow` - Create a shallow copy. May be set to one of the following values:
///   * `symbolic` - symbolic links will be created
///   * `hard` - hard links will be created
///   * `none` - do not make a shallow copy
///   * `Array :modes` - an array of the above, trying the next option on failure
///   * `()` (by using `^shallow`, for instance) - the same as `[hard,symbolic]`
///
/// If the destination is an existing directory, `from` is copied with the same basename into that directory.
/// All destination directories are automatically created.
/// If `from` is a directory, it is recursively copied.
async fn copy(from: types::Path, to: types::Path, (shallow): [_]) -> Value {
    let log = Context::global().log.sublog("fs::copy");

    log.debug(format!(
        "copying {} to {}",
        from.as_ref().0.as_ref().display(),
        to.as_ref().0.as_ref().display()
    ));

    let link_mode = match shallow {
        None => LinkMode::Copy,
        Some(mut v) => {
            try_result!(Context::eval(&mut v).await);
            let source = Source::get(&v);
            match_value! { v,
                types::Unit => LinkMode::Fallback(vec![LinkMode::Hard, LinkMode::Symbolic]),
                types::String(s) => try_result!(s.parse::<LinkMode>().map_err(|e| source.with(e).into_error())),
                types::Array(arr) => {
                    if arr.is_empty() {
                        return source.with("no shallow mode(s) specified").into_error().into();
                    }
                    let mut ret = Vec::new();
                    for v in arr {
                        let v = try_result!(Context::eval_as::<types::String>(v).await);
                        let src = Source::get(&v);
                        let types::String(s) = v.as_ref();
                        ret.push(try_result!(s.parse::<LinkMode>().map_err(|e| src.with(e).into_error())));
                    }
                    LinkMode::Fallback(ret)
                }
                o => return traits::type_error(o, "String, Array, or Unit").into()
            }
        }
    };

    try_result!(recursive_link(
        from.as_ref().0.as_ref(),
        to.as_ref().0.as_ref(),
        &link_mode
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
    types::Bool(path.as_ref().0.as_ref().exists()).into()
}

#[types::ergo_fn]
/// Create a directory (and all ancestor directories).
///
/// Arguments: `(Path :path)`
///
/// Returns a `Unit` value that creates the directory and ancestors if they do not exist.
async fn create_dir(path: types::Path) -> Value {
    try_result!(std::fs::create_dir_all(path.as_ref().0.as_ref())
        .add_context_str(path.as_ref().0.as_ref().display()));
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
    let archive_source = Source::get(&archive);

    let ext: ergo_runtime::Source<String> = match format {
        Some(format) => Source::extract(format).map(|v| v.to_owned().0.into()),
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
                        // We assume the string is not empty (otherwise file_name would have
                        // returned None).
                        archive_source.with(match s.rfind('.') {
                            None => "dir".into(),
                            Some(i) => {
                                // Look for the name ending in one of the supported extensions; splitting
                                // on `.` is not convenient if the filename contains other `.`s.
                                let mut ret = None;
                                let exts = &[".zip", ".tar", ".tar.gz", ".tar.bz2", ".tar.xz"];
                                for ext in exts {
                                    if s.ends_with(ext) {
                                        ret = Some(ext[1..].into());
                                        break;
                                    }
                                }
                                ret.unwrap_or_else(|| s[(i + 1)..].into())
                            }
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
            archive.as_ref().as_ref(),
            &LinkMode::Copy,
        )),
        "zip" => {
            let path = source.as_ref().as_ref();
            let f = try_result!(std::fs::File::create(archive.as_ref().as_ref())
                .add_context_str(archive.as_ref().as_ref().display())
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
                        let p = dir.join(name);
                        let mut f = std::fs::File::open(&p).add_context_str(p.display())?;
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
                .add_context_str(archive.as_ref().as_ref().display())
                .and_then(|p| tar_to(p, &source.as_ref().as_ref()))
                .map_err(|e| ARGS_SOURCE.with(e).into_error()));
        }
        "tar.gz" => {
            use flate2::write::GzEncoder;
            try_result!(std::fs::File::create(archive.as_ref().as_ref())
                .add_context_str(archive.as_ref().as_ref().display())
                .and_then(|p| tar_to(
                    GzEncoder::new(p, Default::default()),
                    &source.as_ref().as_ref()
                ))
                .map_err(|e| ARGS_SOURCE.with(e).into_error()));
        }
        "tar.bz2" => {
            use bzip2::write::BzEncoder;
            try_result!(std::fs::File::create(archive.as_ref().as_ref())
                .add_context_str(archive.as_ref().as_ref().display())
                .and_then(|p| tar_to(
                    BzEncoder::new(p, Default::default()),
                    &source.as_ref().as_ref()
                ))
                .map_err(|e| ARGS_SOURCE.with(e).into_error()));
        }
        "tar.xz" => {
            use xz::write::XzEncoder;
            try_result!(std::fs::File::create(archive.as_ref().as_ref())
                .add_context_str(archive.as_ref().as_ref().display())
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
async fn unarchive(destination: types::Path, mut archive: _) -> Value {
    let to_path = destination.as_ref().as_ref();

    try_result!(Context::eval(&mut archive).await);
    let archive_source = Source::get(&archive);

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
                    let p = to_path.join(file.name());
                    std::fs::create_dir_all(&p).add_context_str(p.display())?;
                } else if file.is_file() {
                    let p = to_path.join(file.name());
                    let parent = p.parent().expect("no parent path in zip output");
                    std::fs::create_dir_all(&parent).add_context_str(parent.display())?;
                    let mut to_file = std::fs::File::create(&p).add_context_str(p.display())?;
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
                try_result!(recursive_link(path, to_path, &LinkMode::Copy));
            } else if path.is_file() {
                try_result!(extract_to(try_result!(std::fs::File::open(&path).add_context_str(path.display())), to_path).map_err(|e| ARGS_SOURCE.with(e.error())));
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
            use futures::io::AsyncReadExt;
            try_result!(bs.read().read_to_end(&mut data).await);
            try_result!(extract_to(std::io::Cursor::new(data), to_path));
        }
        v => return traits::type_error(v, "Path or ByteStream").into()
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
    let mut f = try_result!(std::fs::File::open(file.as_ref().as_ref())
        .add_context_str(file.as_ref().as_ref().display()));
    let mut digest = Sha1::default();
    try_result!(std::io::copy(&mut f, &mut digest));
    use sha::utils::DigestExt;
    types::Bool(digest.to_hex().eq_ignore_ascii_case(sum.as_ref().as_str())).into()
}

#[types::ergo_fn]
/// Make a Path depend on the contents of the file to which it refers.
///
/// Arguments: `(StringOrPath :file)`
///
/// Keyed Arguments:
/// * `:force-check` - if set, always check the given file for a change, ignoring any cached
///   change results from this run. This is useful if a file may have been changed while a script
///   is executing.
///
/// Returns a `Path` that is identified by the contents of the file to which the argument refers.
/// `file` must exist and refer to a file.
async fn track(mut file: _, (force_check): [_]) -> Value {
    try_result!(Context::eval(&mut file).await);
    let file = match_value! {file,
        types::String(s) => s.as_str().into(),
        types::Path(p) => p.into_pathbuf(),
        v => return traits::type_error(v, "String or Path").into()
    };
    let force_check = force_check.is_some();

    let loaded_info = try_result!(Context::global().shared_state.get(|| {
        let store = Context::global().store.item(item_name!("track"));
        LoadedTrackInfo::new(store)
    }));

    let hash = {
        let file_entry = loaded_info.info.get(file.clone());
        let mut guard = file_entry.lock().await;
        match &mut *guard {
            Some((_, Some(v))) if !force_check => *v,
            other => {
                let meta = try_result!(std::fs::metadata(&file).add_context_str(file.display()));
                let modification_time =
                    try_result!(meta.modified().add_context_str(file.display()));

                let calc_hash = other
                    .as_ref()
                    .map(|(data, _)| data.modification_time < modification_time)
                    .unwrap_or(true);

                if calc_hash {
                    let f = try_result!(std::fs::File::open(&file).add_context_str(file.display()));
                    let content_hash = try_result!(
                        ergo_runtime::hash::hash_read(f).add_context_str(file.display())
                    );
                    *other = Some((
                        FileData {
                            modification_time,
                            content_hash,
                        },
                        Some(content_hash),
                    ));
                    content_hash
                } else {
                    let inner = other.as_mut().unwrap();
                    inner.1 = Some(inner.0.content_hash);
                    inner.0.content_hash
                }
            }
        }
    };

    let mut v: Value = types::Path::from(file).into();
    v.set_dependencies(depends![v.id(), hash]);
    v
}

/// A HashMap which allows insertion+retrieval of values by reference.
struct RefHashMap<K, V> {
    inner: std::sync::Mutex<std::collections::HashMap<K, Box<V>>>,
}

impl<K, V> Default for RefHashMap<K, V> {
    fn default() -> Self {
        RefHashMap {
            inner: Default::default(),
        }
    }
}

impl<K: Eq + std::hash::Hash, V> std::iter::FromIterator<(K, V)> for RefHashMap<K, V> {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = (K, V)>,
    {
        RefHashMap {
            inner: std::sync::Mutex::new(iter.into_iter().map(|(k, v)| (k, Box::new(v))).collect()),
        }
    }
}

impl<K, V> RefHashMap<K, V> {
    pub fn into_entries(self) -> Vec<(K, V)> {
        self.inner
            .into_inner()
            .unwrap()
            .into_iter()
            .map(|(k, v)| (k, *v))
            .collect()
    }
}

impl<K: Eq + std::hash::Hash, V: Default> RefHashMap<K, V> {
    pub fn get(&self, key: K) -> &V {
        let v =
            std::ptr::NonNull::from(self.inner.lock().unwrap().entry(key).or_default().as_ref());
        // Safety: We only support adding entries to the hashmap, and as long as a reference is
        // maintained the value will be present.
        unsafe { v.as_ref() }
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct FileData {
    modification_time: std::time::SystemTime,
    content_hash: u128,
}

#[derive(ErgoType)]
struct LoadedTrackInfo {
    pub info:
        RefHashMap<std::path::PathBuf, futures::lock::Mutex<Option<(FileData, Option<u128>)>>>,
    item: ergo_runtime::context::Item,
}

impl LoadedTrackInfo {
    pub fn new(item: ergo_runtime::context::Item) -> ergo_runtime::Result<Self> {
        let info = if item.exists() {
            let content = item.read()?;
            let stored: Vec<(std::path::PathBuf, FileData)> =
                bincode::deserialize_from(content).map_err(|e| e.to_string())?;
            stored
                .into_iter()
                .map(|(k, v)| (k, futures::lock::Mutex::new(Some((v, None)))))
                .collect()
        } else {
            Default::default()
        };
        Ok(LoadedTrackInfo { info, item })
    }
}

impl std::ops::Drop for LoadedTrackInfo {
    fn drop(&mut self) {
        let content = match self.item.write() {
            Err(e) => {
                eprintln!("could not open fs::track info for writing: {}", e);
                return;
            }
            Ok(v) => v,
        };
        let entries = std::mem::take(&mut self.info).into_entries();
        let entries: Vec<(std::path::PathBuf, FileData)> = entries
            .into_iter()
            .filter_map(|(k, v)| v.into_inner().map(|v| (k, v.0)))
            .collect();
        if let Err(e) = bincode::serialize_into(content, &entries) {
            eprintln!("error while serializing fs::track info: {}", e);
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
    let path = path.as_ref().as_ref();
    if path.is_file() {
        try_result!(std::fs::remove_file(&path).add_context_str(path.display()));
    } else if path.is_dir() {
        try_result!(std::fs::remove_dir_all(&path).add_context_str(path.display()));
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
    let path = file.as_ref().as_ref();
    // TODO don't hash here, leave that to the script writer?
    let hash = try_result!(ergo_runtime::hash::hash_read(try_result!(
        std::fs::File::open(&path).add_context_str(path.display())
    )));
    Value::constant_deps(
        types::ByteStream::new(io::Blocking::new(try_result!(
            std::fs::File::open(&path).add_context_str(path.display())
        ))),
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
    let bytes = try_result!(traits::into::<types::ByteStream>(bytes).await);

    let mut f = io::Blocking::new(try_result!(std::fs::File::create(file.as_ref().as_ref())
        .add_context_str(file.as_ref().as_ref().display())));
    try_result!(ergo_runtime::io::copy(&mut bytes.as_ref().read(), &mut f).await);
    types::Unit.into()
}

#[types::ergo_fn]
/// Append a ByteStream to a file.
///
/// Arguments: `(Path :file) (Into<ByteStream> :bytes)`
/// Creates or appends the file with the bytes from the ByteStream.
async fn append(file: types::Path, bytes: _) -> Value {
    let bytes = try_result!(traits::into::<types::ByteStream>(bytes).await);

    let mut f = io::Blocking::new(try_result!(std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(file.as_ref().as_ref())
        .add_context_str(file.as_ref().as_ref().display())));
    try_result!(ergo_runtime::io::copy(&mut bytes.as_ref().read(), &mut f).await);
    types::Unit.into()
}
