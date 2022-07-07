//! Filesystem runtime functions.

use ergo_runtime::{
    depends,
    error::{Diagnostic, DiagnosticInfo},
    io,
    metadata::Source,
    traits,
    type_system::ErgoType,
    types,
    value::match_value,
    Context, Value,
};
use glob::glob;
use sha::sha1::Sha1;
use std::path::Path;

pub fn module() -> Value {
    crate::make_string_map! {
        "append" = append(),
        "archive" = archive(),
        "copy" = copy(),
        "create-dir" = create_dir(),
        "file-size" = file_size(),
        "file-type" = file_type(),
        "glob" = glob_(),
        "lock" = lock(),
        "read" = read(),
        "remove" = remove(),
        "rename" = rename(),
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
    let pattern = traits::into::<types::String>(pattern).await?;
    let pattern_source = Source::get(&pattern);

    let mut path = Context::source_path(&ARGS_SOURCE)
        .map(|p| p.parent().unwrap().into())
        .unwrap_or_else(|| std::env::current_dir().unwrap());
    path.push(pattern.as_ref().as_str());

    match glob(path.to_str().unwrap()) {
        Err(e) => pattern_source.with(e).into_error().into(),
        Ok(paths) => {
            let paths: Result<Vec<std::path::PathBuf>, glob::GlobError> = paths.collect();
            let paths: Vec<Value> = paths
                .map_err(|e| {
                    ergo_runtime::error! {
                        labels: [ primary(pattern_source.with("while evaluating this glob")) ],
                        error: e
                    }
                })?
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
    follow_symlinks: bool,
) -> Result<(), ergo_runtime::error::Diagnostic> {
    let to = to.as_ref();
    let from = from.as_ref();
    if to.is_dir() {
        let mut to = to.to_owned();
        to.push(from.file_name().set_message("'from' path ends in ..")?);
        return recursive_link(from, &to, mode, follow_symlinks);
    }

    if to.is_file() {
        std::fs::remove_file(to).add_note(format_args!("path was {}", to.display()))?;
    }

    let meta = if follow_symlinks {
        std::fs::metadata(from)
    } else {
        std::fs::symlink_metadata(from)
    }
    .add_note(format_args!("path was {}", from.display()))?;
    if meta.is_dir() {
        if mode == &LinkMode::Symbolic {
            symlink_dir(from, to).add_note(format_args!(
                "while creating symbolic link pointing to {} at {}",
                from.display(),
                to.display()
            ))?;
        } else {
            std::fs::create_dir_all(to).add_note(format_args!("directory was {}", to.display()))?;
            for d in from
                .read_dir()
                .add_note(format_args!("path was {}", from.display()))?
            {
                let d = d.add_note(format_args!("path was {}", from.display()))?;
                let name = d.file_name();
                let mut to = to.to_owned();
                to.push(name);
                recursive_link(d.path(), &to, mode, follow_symlinks)?;
            }
        }
        Ok(())
    } else {
        if let Some(parent) = to.parent() {
            std::fs::create_dir_all(parent)
                .add_note(format_args!("directory was {}", parent.display()))?;
        }
        let is_symlink = meta.file_type().is_symlink() && !follow_symlinks;

        fn do_copy(
            from: &Path,
            to: &Path,
            mode: &LinkMode,
            from_is_symlink: bool,
        ) -> Result<(), ergo_runtime::error::Diagnostic> {
            match mode {
                LinkMode::Copy => if from_is_symlink {
                    let is_dir = from.is_dir();
                    std::fs::read_link(from).and_then(
                        |original| if is_dir { symlink_dir } else { symlink_file }(original, to),
                    )
                } else {
                    std::fs::copy(from, to).map(|_| ())
                }
                .add_note(format_args!(
                    "while copying {} to {}",
                    from.display(),
                    to.display()
                )),
                LinkMode::Hard => std::fs::hard_link(from, to).add_note(format_args!(
                    "while creating a hard link of {} at {}",
                    from.display(),
                    to.display()
                )),
                LinkMode::Symbolic => symlink_file(from, to).add_note(format_args!(
                    "while creating symbolic link pointing to {} at {}",
                    from.display(),
                    to.display()
                )),
                LinkMode::Fallback(modes) => {
                    let mut last_result = Ok(());
                    for m in modes {
                        last_result = do_copy(from, to, m, from_is_symlink);
                        if last_result.is_ok() {
                            return Ok(());
                        }
                    }
                    last_result
                }
            }
        }

        do_copy(from, to, mode, is_symlink)
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
/// * `:follow-symlinks` - If present, follow symlinks when copying files and directories.
///
/// If the destination is an existing directory, `from` is copied with the same basename into that
/// directory.
///
/// All destination directories are automatically created.
///
/// If `from` is a directory, it is recursively copied.
///
/// If `shallow` is `symbolic` and `from` doesn't exist, it is assumed that `from` is a path
/// relative to `to` and a symbolic file link will be created (relevant on windows, where symbolic
/// file links and symbolic directory links differ).
async fn copy(from: types::Path, to: types::Path, (shallow): [_], (follow_symlinks): [_]) -> Value {
    let log = Context::global().log.sublog("fs:copy");

    log.debug(format!(
        "copying {} to {}",
        from.as_ref().display(),
        to.as_ref().display()
    ));

    let follow_symlinks = follow_symlinks.is_some();

    let link_mode = match shallow {
        None => LinkMode::Copy,
        Some(mut v) => {
            Context::eval(&mut v).await?;
            let source = Source::get(&v);
            match_value! { v,
                types::Unit => LinkMode::Fallback(vec![LinkMode::Hard, LinkMode::Symbolic]),
                types::String(s) => s.parse::<LinkMode>().map_err(|e| source.with(e).into_error())?,
                types::Array(arr) => {
                    if arr.is_empty() {
                        Err(source.with("no shallow mode(s) specified").into_error())?;
                    }
                    let mut ret = Vec::new();
                    for v in arr {
                        let v = Context::eval_as::<types::String>(v).await?;
                        let src = Source::get(&v);
                        let types::String(s) = v.as_ref();
                        ret.push(s.parse::<LinkMode>().map_err(|e| src.with(e).into_error())?);
                    }
                    LinkMode::Fallback(ret)
                }
                o => Err(traits::type_error(o, "String, Array, or Unit"))?
            }
        }
    };

    let from = from.as_ref().as_ref();
    let to = to.as_ref().as_ref();

    // Special handling for the case where you are creating a symlink with a path relative to the
    // target, assuming it is a file rather than a directory (relevant on windows).
    if link_mode == LinkMode::Symbolic && !from.exists() {
        symlink_file(&from, &to).add_note(format_args!(
            "while creating symbolic link pointing to {} at {}",
            from.display(),
            to.display()
        ))
    } else {
        let from: std::path::PathBuf = from.into();
        let to: std::path::PathBuf = to.into();
        Context::global()
            .task
            .spawn_blocking(move || recursive_link(from, to, &link_mode, follow_symlinks))
            .await?
    }
    .add_primary_label(ARGS_SOURCE.with(""))?;

    types::Unit.into()
}

#[types::ergo_fn]
/// Return the file size of the given path.
///
/// Arugments: `(Path :path)`
///
/// Returns a Number with the file size in bytes. Does not follow symlinks (returning the size of
/// the symlink file).
///
/// If the path does not exist or the user does not have permission to the path, returns `Unset`.
async fn file_size(path: types::Path) -> Value {
    std::fs::symlink_metadata(path.as_ref().as_ref())
        .map(|m| types::Number::from(m.len()).into())
        .unwrap_or(types::Unset.into())
}

#[types::ergo_fn]
/// Return the file type of the given path.
///
/// Arugments: `(Path :path)`
///
/// Returns a String with the file type:
/// * `file` - a normal file
/// * `directory` - a directory
/// * `symlink` - a symbolic link
/// * `other` - the file is none of the above (but exists)
///
/// If the path does not exist or the user does not have permission to the path, returns `Unset`.
async fn file_type(path: types::Path) -> Value {
    std::fs::symlink_metadata(path.as_ref().as_ref())
        .map(|m| {
            types::String::from(if m.file_type().is_dir() {
                "directory"
            } else if m.file_type().is_file() {
                "file"
            } else if m.file_type().is_symlink() {
                "symlink"
            } else {
                "other"
            })
            .into()
        })
        .unwrap_or(types::Unset.into())
}

#[types::ergo_fn]
/// Create a directory (and all ancestor directories).
///
/// Arguments: `(Path :path)`
///
/// Returns a `Unit` value that creates the directory and ancestors if they do not exist.
async fn create_dir(path: types::Path) -> Value {
    std::fs::create_dir_all(path.as_ref().as_ref())
        .add_note(format_args!("directory was {}", path.as_ref().display()))?;
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
        Some(format) => Source::extract(format).map(|v| v.into_owned().0.into()),
        None => {
            let path = archive.as_ref().as_ref();
            if path.starts_with(source.as_ref().as_ref()) {
                Err(archive_source
                    .with("archive path lies within archive source path")
                    .into_error())?;
            }
            match path.file_name() {
                None => Err(archive_source.with("invalid archive path").into_error())?,
                Some(f) => match f.to_str() {
                    None => Err(archive_source
                        .with("invalid unicode in filename")
                        .into_error())?,
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

    let result = match ext.as_str() {
        "dir" => {
            let source = source.clone();
            let archive = archive.clone();
            Context::global()
                .task
                .spawn_blocking(move || {
                    recursive_link(
                        source.as_ref().as_ref(),
                        archive.as_ref().as_ref(),
                        &LinkMode::Copy,
                        false,
                    )
                })
                .await?
        }
        "zip" => {
            fn add_dir_entries<W: std::io::Write + std::io::Seek>(
                dir: &Path,
                prefix: &Path,
                zip: &mut zip::ZipWriter<W>,
            ) -> Result<(), ergo_runtime::error::Diagnostic> {
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
                        let mut f = std::fs::File::open(&p)
                            .add_note(format_args!("path was {}", p.display()))?;
                        std::io::copy(&mut f, zip)?;
                    }
                }
                Ok(())
            }

            let path = source.as_ref().as_ref();
            std::fs::File::create(archive.as_ref().as_ref())
                .into_diagnostic()
                .and_then(|f| {
                    let mut writer = zip::ZipWriter::new(f);
                    add_dir_entries(&path, &std::path::PathBuf::default(), &mut writer)?;
                    writer.finish().map(|_| ()).into_diagnostic()
                })
        }
        "tar" => std::fs::File::create(archive.as_ref().as_ref())
            .and_then(|p| tar_to(p, &source.as_ref().as_ref()))
            .map(|_| ())
            .into_diagnostic(),
        "tar.gz" => {
            use flate2::write::GzEncoder;
            std::fs::File::create(archive.as_ref().as_ref())
                .and_then(|p| {
                    tar_to(
                        GzEncoder::new(p, Default::default()),
                        &source.as_ref().as_ref(),
                    )
                })
                .map(|_| ())
                .into_diagnostic()
        }
        "tar.bz2" => {
            use bzip2::write::BzEncoder;
            std::fs::File::create(archive.as_ref().as_ref())
                .and_then(|p| {
                    tar_to(
                        BzEncoder::new(p, Default::default()),
                        &source.as_ref().as_ref(),
                    )
                })
                .map(|_| ())
                .into_diagnostic()
        }
        "tar.xz" => {
            use xz::write::XzEncoder;
            std::fs::File::create(archive.as_ref().as_ref())
                .and_then(|p| {
                    tar_to(
                        XzEncoder::new(p, Default::default()),
                        &source.as_ref().as_ref(),
                    )
                })
                .map(|_| ())
                .into_diagnostic()
        }
        o => Err(Diagnostic::from(format!("unsupported format: {}", o))
            .add_primary_label(ext_source.with("")))?,
    };

    result
        .add_primary_label(ARGS_SOURCE.with("while creating this archive"))
        .add_note(format_args!(
            "archive path was {}",
            archive.as_ref().display()
        ))
        .add_note(format_args!(
            "source path was {}",
            source.as_ref().display()
        ))?;

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

    Context::eval(&mut archive).await?;
    let archive_source = Source::get(&archive);

    use std::io::{Read, Seek};

    fn extract_to<T: Read + Seek, P: AsRef<Path>>(
        mut archive: T,
        to_path: P,
    ) -> Result<(), ergo_runtime::error::Diagnostic> {
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
                    std::fs::create_dir_all(&p)
                        .add_note(format_args!("directory was {}", p.display()))?;
                } else if file.is_file() {
                    let p = to_path.join(file.name());
                    let parent = p.parent().expect("no parent path in zip output");
                    std::fs::create_dir_all(&parent)
                        .add_note(format_args!("directory was {}", parent.display()))?;
                    let mut to_file = std::fs::File::create(&p)
                        .add_note(format_args!("path was {}", p.display()))?;
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
        p@types::Path { .. } => {
            let path = p.as_ref();
            if path.is_dir() {
                let path_c: std::path::PathBuf = path.clone().into();
                let to_path_c: std::path::PathBuf = to_path.clone().into();
                Context::global().task.spawn_blocking(move ||
                    recursive_link(path_c, to_path_c, &LinkMode::Copy, false)
                ).await?
                    .add_primary_label(ARGS_SOURCE.with("while copying directory"))
                    .add_note(format_args!("source path was {}", path.display()))
                    .add_note(format_args!("target path was {}", to_path.display()))?;
            } else if path.is_file() {
                let f = std::fs::File::open(&path)
                            .add_note(format_args!("archive path was {}", path.display()))
                            .add_primary_label(archive_source.with("while opening this archive"))?;
                let to_path_c: std::path::PathBuf = to_path.clone().into();
                Context::global().task.spawn_blocking(move || extract_to(f, to_path_c)).await?
                    .add_note(format_args!("archive path was {}", path.display()))
                    .add_note(format_args!("target path was {}", to_path.display()))
                    .add_primary_label(archive_source.with("while extracting this archive"))?;
            }
            else {
                Err(Diagnostic::from("path is not a file nor directory").add_primary_label(archive_source.with("")))?;
            }
        }
        bs@types::ByteStream {..} => {
            // Read the entire byte stream into memory (no AsyncRead support in decoders)
            let mut data = Vec::new();
            use futures::io::AsyncReadExt;
            bs.read().read_to_end(&mut data).await
                .add_primary_label(archive_source.with("while reading this byte stream"))?;
            let to_path_c: std::path::PathBuf = to_path.clone().into();
            Context::global().task.spawn_blocking(move || extract_to(std::io::Cursor::new(data), to_path_c)).await?
                .add_note(format_args!("target path was {}", to_path.display()))
                .add_primary_label(archive_source.with("while extracting this archive"))?;
        }
        v => Err(traits::type_error(v, "Path or ByteStream"))?,
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
    let mut f = std::fs::File::open(file.as_ref().as_ref())
        .add_note(format_args!("path was {}", file.as_ref().display()))?;
    let mut digest = Sha1::default();
    std::io::copy(&mut f, &mut digest)
        .add_note(format_args!("path was {}", file.as_ref().display()))?;
    use sha::utils::DigestExt;
    types::Bool(digest.to_hex().eq_ignore_ascii_case(sum.as_ref().as_str())).into()
}

#[types::ergo_fn]
#[eval_for_id]
/// Make a Path depend on the contents of the file to which it refers.
///
/// Arguments: `(Into<Path> :file)`
///
/// Keyed Arguments:
/// * `:force-check` - if set, always check the given file for a change, ignoring any cached
///   change results from this run. This is useful if a file may have been changed while a script
///   is executing.
///
/// Returns a `Path` that is identified by the contents of the file to which the argument refers.
/// `file` must exist and refer to a file.
async fn track(file: _, (force_check): [_]) -> Value {
    let file = traits::into::<types::Path>(file)
        .await?
        .into_owned()
        .into_pathbuf();
    let force_check = force_check.is_some();

    let loaded_info = Context::global().shared_state.get(|| {
        let std_dir = Context::global().env.project_directory().join("std");
        std::fs::create_dir_all(&std_dir)
            .add_note(format_args!("path was {}", std_dir.display()))?;
        let log = Context::global().log.sublog("fs:track");
        LoadedTrackInfo::new(std_dir.join("track"), log)
    })?;

    let hash = {
        let file_entry = loaded_info.info.get(file.clone());
        let mut guard = file_entry.lock().await;
        match &mut *guard {
            Some(FileData {
                computed_content_hash: true,
                content_hash,
                ..
            }) if !force_check => *content_hash,
            other => {
                ergo_runtime::error_info!(notes: [format_args!("path was {}", file.display())], {
                    let meta = std::fs::metadata(&file)?;
                    let modification_time = meta.modified()?;

                    let calc_hash = other
                        .as_ref()
                        .map(|data| data.modification_time < modification_time)
                        .unwrap_or(true);

                    ergo_runtime::Result::Ok(if calc_hash {
                        let f = std::fs::File::open(&file)?;
                        let content_hash = ergo_runtime::hash::hash_read(f)?;
                        *other = Some(FileData {
                            modification_time,
                            content_hash,
                            computed_content_hash: true,
                        });
                        content_hash
                    } else {
                        let inner = other.as_mut().unwrap();
                        inner.computed_content_hash = true;
                        inner.content_hash
                    })
                })?
            }
        }
    };

    let mut v: Value = types::Path::from(file).into();
    v.set_identity(depends![dyn v, hash]);
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

#[derive(Debug)]
struct FileData {
    modification_time: std::time::SystemTime,
    content_hash: u128,
    computed_content_hash: bool,
}

#[derive(ErgoType)]
struct LoadedTrackInfo {
    pub info: RefHashMap<std::path::PathBuf, futures::lock::Mutex<Option<FileData>>>,
    persist: std::path::PathBuf,
    log: ergo_runtime::context::Log,
}

impl LoadedTrackInfo {
    pub fn new(
        persist: std::path::PathBuf,
        log: ergo_runtime::context::Log,
    ) -> ergo_runtime::Result<Self> {
        let mut entries = Vec::new();

        let result = ergo_runtime::error_info! {{
            use crate::sqlite::{Connection, Duration, U128, State};
            use std::time::{SystemTime};

            let conn = Connection::open(&persist)?;
            conn.execute("CREATE TABLE IF NOT EXISTS tracked_files (
                path TEXT NOT NULL,
                mod_time_secs INTEGER NOT NULL,
                mod_time_nsecs INTEGER NOT NULL,
                content_u8 INTEGER NOT NULL, content_l8 INTEGER NOT NULL
            )")?;
            let mut stmt = conn.prepare("SELECT path, mod_time_secs, mod_time_nsecs, content_u8, content_l8 FROM tracked_files")?;
            while let State::Row = stmt.next()? {
                let path: String = stmt.read(0)?;
                let Duration(mod_time) = stmt.read(1)?;
                let U128(content_hash) = stmt.read(3)?;
                entries.push((
                    std::path::PathBuf::from(path),
                    SystemTime::UNIX_EPOCH + mod_time,
                    content_hash,
                ));
            }
            ergo_runtime::Result::Ok(())
        }};

        if let Err(e) = result {
            log.error(format_args!("error reading file tracking database: {}", e));
        }

        let info = entries
            .into_iter()
            .map(|(path, modification_time, content_hash)| {
                (
                    path,
                    futures::lock::Mutex::new(Some(FileData {
                        modification_time,
                        content_hash,
                        computed_content_hash: false,
                    })),
                )
            })
            .collect();

        Ok(LoadedTrackInfo { info, persist, log })
    }
}

impl std::ops::Drop for LoadedTrackInfo {
    fn drop(&mut self) {
        let result = ergo_runtime::error_info! {{
            use crate::sqlite::{Connection, Duration, U128, Transaction, State};
            use std::time::SystemTime;

            let conn = Connection::open(&self.persist)?;
            let transaction = Transaction::new(&conn)?;
            conn.execute("DELETE FROM tracked_files")?;
            for (path, data) in std::mem::take(&mut self.info)
                .into_entries()
                .into_iter()
                .filter_map(|(k, v)| v.into_inner().map(|v| (k,v)))
            {
                match (path.to_str(), data.modification_time.duration_since(SystemTime::UNIX_EPOCH)) {
                    (Some(path), Ok(dur)) => {
                        let mut stmt = conn.prepare("INSERT INTO tracked_files (
                            path, mod_time_secs, mod_time_nsecs, content_u8, content_l8
                        ) VALUES (?,?,?,?,?)")?;
                        stmt.bind(1, path)?;
                        stmt.bind(2, Duration(dur))?;
                        stmt.bind(4, U128(data.content_hash))?;
                        while stmt.next()? != State::Done {}
                    }
                    _ => ()
                }
            }
            transaction.commit()
        }};
        if let Err(e) = result {
            self.log
                .error(format_args!("error writing file tracking database: {}", e));
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
    ergo_runtime::error_info!(notes: [format_args!("path was {}", path.display())], {
        if path.is_file() {
            std::fs::remove_file(&path)?;
        } else if path.is_dir() {
            std::fs::remove_dir_all(&path)?;
        }
        ergo_runtime::Result::Ok(())
    })?;
    types::Unit.into()
}

#[types::ergo_fn]
/// Rename a file or directory.
///
/// Arguments: `(Path :from) (Path :to)`
///
/// If `from` does not exist, the user doesn't have permissions to `from`, or `from` and `to` are
/// on separate filesystems, an `Error` is returned.
///
/// If `to` already exists, it is overwritten.
async fn rename(from: types::Path, to: types::Path) -> Value {
    let from = from.as_ref().as_ref();
    let to = to.as_ref().as_ref();
    ergo_runtime::error_info!(
        notes: [
            format_args!("`from` was {}", from.display()),
            format_args!("`to` was {}", to.display())
        ], {
        std::fs::rename(&from, &to)
    })?;
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
    ergo_runtime::error_info!(notes: [format_args!("path was {}", path.display())], {
        let hash = ergo_runtime::hash::hash_read(std::fs::File::open(&path)?)?;
        ergo_runtime::Result::Ok(Value::with_id(
            types::ByteStream::new(io::Blocking::new(std::fs::File::open(&path)?)),
            depends![const hash],
        ))
    })?
}

#[types::ergo_fn]
/// Write a ByteStream to a file.
///
/// Arguments: `(Path :file) (Into<ByteStream> :bytes)`
///
/// Creates or overwrites the file with the bytes from the ByteStream.
async fn write(file: types::Path, bytes: _) -> Value {
    let bytes = traits::into::<types::ByteStream>(bytes).await?;
    let path = file.as_ref().as_ref();

    ergo_runtime::error_info!(
        notes: [format_args!("target path was {}", path.display())],
        async {
            let mut f = io::Blocking::new(std::fs::File::create(&path)?);
            ergo_runtime::io::copy(&mut bytes.as_ref().read(), &mut f).await
        }
    )?;
    types::Unit.into()
}

#[types::ergo_fn]
/// Append a ByteStream to a file.
///
/// Arguments: `(Path :file) (Into<ByteStream> :bytes)`
/// Creates or appends the file with the bytes from the ByteStream.
async fn append(file: types::Path, bytes: _) -> Value {
    let bytes = traits::into::<types::ByteStream>(bytes).await?;
    let path = file.as_ref().as_ref();

    ergo_runtime::error_info!(
        notes: [format_args!("target path was {}", path.display())],
        async {
            let mut f = io::Blocking::new(
                std::fs::OpenOptions::new()
                    .create(true)
                    .append(true)
                    .open(&path)?,
            );
            ergo_runtime::io::copy(&mut bytes.as_ref().read(), &mut f).await?;
            ergo_runtime::Result::Ok(())
        }
    )?;
    types::Unit.into()
}

#[types::ergo_fn]
/// Acquire a lock on a file while evaluating a value.
///
/// Arguments: `(Path :file) :value`
///
/// Keyed Arguments:
/// * `shared` - if present, a shared lock (rather than an exclusive lock) will be acquired.
///
/// `file` will be created if it does not exist, otherwise it must be readable and writable by the
/// user.
///
/// **Note**: this only acquires advisory locks, so a process _not_ acquiring a lock before
/// accessing a file will still be able to do so. This acquires per-process locks, so acts more
/// like a recursive lock wrt each process: if you call `std:fs:lock` on the same file in the same
/// ergo evaluation process, it will succeed. It is primarily for deconflicting access among
/// separate processes. Also, on unix systems it acquires a POSIX lock, which may not interact with
/// old-style FLOCK locks at all.
///
/// Returns the result of evaluating `value`.
async fn lock(file: types::Path, mut value: _, (shared): [_]) -> Value {
    use std::fs::{File, OpenOptions};

    let shared = shared.is_some();
    let f = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .open(file.as_ref().as_ref())
        .add_note(format_args!("file was {}", file.as_ref().display()))?;

    /// Simple wrapper to allow `file_guard::lock` to use `std::ops::Deref` while still passing
    /// ownership to the closure and back.
    struct OwnedFile(File);
    impl std::ops::Deref for OwnedFile {
        type Target = File;
        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    let guard = Context::global()
        .task
        .spawn_blocking(move || {
            file_guard::lock(
                OwnedFile(f),
                if shared {
                    file_guard::Lock::Shared
                } else {
                    file_guard::Lock::Exclusive
                },
                0,
                1,
            )
            .add_note(format_args!("file was {}", file.as_ref().display()))
        })
        .await??;
    Context::eval(&mut value).await?;
    drop(guard);
    value
}
