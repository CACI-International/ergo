use std::path::Path;
use std::process::Command;

const PROGRAM: &'static str = env!("CARGO_BIN_EXE_ergo");
const MANIFEST: &'static str = env!("CARGO_MANIFEST_DIR");

type Result<T> = std::io::Result<T>;

fn run(args: &[&str], word: &[u8]) -> Result<()> {
    Command::new(PROGRAM)
        .args(args)
        .current_dir(
            Path::new(MANIFEST)
                .parent()
                .unwrap()
                .join(file!())
                .with_extension(""),
        )
        .output()
        .and_then(|output| {
            if output.status.success() {
                if output.stdout == word {
                    Ok(())
                } else {
                    Err(std::io::Error::new(
                        std::io::ErrorKind::Other,
                        format!(
                            "invalid output result: {}",
                            String::from_utf8_lossy(&output.stdout)
                        ),
                    ))
                }
            } else {
                Err(std::io::Error::new(
                    std::io::ErrorKind::Other,
                    format!(
                        "process returned failure exit status\nstdout: {}\nstderr: {}",
                        String::from_utf8_lossy(&output.stdout),
                        String::from_utf8_lossy(&output.stderr)
                    ),
                ))
            }
        })
}

macro_rules! typical_test {
    ( $name:ident, $arg:expr ) => {
        #[test]
        fn $name() -> Result<()> {
            run(&[$arg], concat!(stringify!($name), "\n").as_bytes())
        }
    };

    ( $name:ident ) => {
        typical_test!($name, stringify!($name));
    };
}

typical_test!(basic_no_ext); // Loading a file (ergo X => {X})
typical_test!(basic_ext); // Loading with an extension (ergo X => {X.ergo})
typical_test!(directory); // Loading a directory (ergo X => {X/dir.ergo})
typical_test!(directory_ext); // Loading a directory with an extension (ergo X => {X.ergo/dir.ergo})
typical_test!(workspace_load, "workspace_load/workspace.ergo"); // Loading a sibling from a workspace
typical_test!(workspace_access, "workspace_access/subdir/mod"); // Accessing a workspace from an child
typical_test!(workspace_command); // Fallback to `workspace:command` when loading fails (ergo X => workspace:command X)
typical_test!(workspace_parent, "workspace_parent/subdir/workspace.ergo"); // Loading a parent workspace from a workspace
