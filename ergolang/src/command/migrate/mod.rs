//! Migrations of syntax from older versions.

use ergo_runtime::{context::Sources, Source};
use ropey::Rope;
use std::borrow::Cow;
use std::collections::HashMap;
use std::fs::File;
use std::path::PathBuf;

mod parse;
mod parse_tree;
mod tokenize;

#[derive(Debug, clap::Args)]
pub struct Migrate {
    #[clap(short, long)]
    /// Apply changes to files.
    pub apply: bool,
    /// Files to migrate. If no files are specified, stdin is read and the migrated script is written to stdout.
    pub files: Vec<PathBuf>,
}

type Str = Cow<'static, str>;

enum MigrationLevel {
    Syntactic,
    Semantic,
}

struct Migration {
    pub source: Source<()>,
    pub message: Str,
    pub level: MigrationLevel,
    pub change: Option<Str>,
}

impl Migration {
    pub fn new<Msg: Into<Str>, Change: Into<Str>>(
        source: Source<()>,
        message: Msg,
        level: MigrationLevel,
        change: Option<Change>,
    ) -> Self {
        Migration {
            source,
            message: message.into(),
            level,
            change: change.map(|c| c.into()),
        }
    }

    pub fn syntax<Msg: Into<Str>, Change: Into<Str>>(
        source: Source<()>,
        message: Msg,
        change: Option<Change>,
    ) -> Self {
        Self::new(source, message, MigrationLevel::Syntactic, change)
    }

    pub fn semantics<Msg: Into<Str>, Change: Into<Str>>(
        source: Source<()>,
        message: Msg,
        change: Change,
    ) -> Self {
        Self::new(source, message, MigrationLevel::Semantic, Some(change))
    }

    pub fn semantics_warn<Msg: Into<Str>>(source: Source<()>, message: Msg) -> Self {
        Self::new(
            source,
            message,
            MigrationLevel::Semantic,
            None as Option<&'static str>,
        )
    }
}

impl super::Command for Migrate {
    fn run(self) -> Result<(), String> {
        let sources = Sources::new();
        let mut migrations = Vec::new();
        for f in self.files {
            let source_id = sources.add_file(f).map_err(|e| e.to_string())?;
            let source = Source::new(source_id).with(sources.content(source_id).unwrap());
            let tokens = tokenize::Tokens::from(source);
            let parser = parse_tree::Parser::from(tokens);
            if let Ok(m) = parse::migrations(parser) {
                migrations.extend(m);
            }
        }

        let diagnostics: Vec<_> = migrations
            .iter()
            .map(
                |Migration {
                     source,
                     message,
                     level,
                     ..
                 }| {
                    use ergo_runtime::error::{Diagnostic, Label, Severity};
                    Diagnostic {
                        severity: match level {
                            MigrationLevel::Syntactic => Severity::Note,
                            MigrationLevel::Semantic => Severity::Warning,
                        },
                        message: message.clone().into_owned().into(),
                        labels: vec![Label::primary(source.with(""))].into(),
                        notes: Default::default(),
                    }
                },
            )
            .collect();

        let mut new_files: HashMap<ergo_runtime::context::SourceId, Rope> = HashMap::new();

        if self.apply {
            migrations.reverse();
            for Migration { source, change, .. } in migrations {
                if let Some(change) = change {
                    let rope = new_files
                        .entry(source.source_id)
                        .or_insert_with(|| sources.content(source.source_id).unwrap().into());
                    let start = rope.byte_to_char(source.location.start);
                    let end = rope.byte_to_char(source.location.end());
                    rope.remove(start..end);
                    rope.insert(start, &change);
                }
            }
            for (id, s) in new_files {
                let f = File::create(sources.path(id).unwrap()).map_err(|e| e.to_string())?;
                s.write_to(f).map_err(|e| e.to_string())?;
            }
            print!(
                "{}",
                ergo_runtime::error::diagnostics_to_string(
                    diagnostics
                        .iter()
                        .filter(|d| d.severity == ergo_runtime::error::Severity::Warning),
                    &sources,
                    true
                )
            );
        } else {
            print!(
                "{}",
                ergo_runtime::error::diagnostics_to_string(&diagnostics, &sources, true)
            );
        }

        Ok(())
    }
}
