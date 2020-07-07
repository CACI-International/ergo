use std::path::Path;
use std::process::Command;

const PROGRAM: &'static str = env!("CARGO_BIN_EXE_so");

type Result<T> = std::io::Result<T>;

fn run(args: &[&str], word: &[u8]) -> Result<()> {
    Command::new(PROGRAM)
        .args(args)
        .current_dir(Path::new(file!()).with_extension(""))
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

typical_test!(basic_no_ext);
typical_test!(basic_ext);
typical_test!(directory);
typical_test!(directory_ext);
typical_test!(workspace_load);
typical_test!(prelude_load, "prelude_load/subdir/submod");
typical_test!(workspace_fallback, "workspace_fallback/mod");
typical_test!(workspace_parent, "workspace_parent/subdir");
