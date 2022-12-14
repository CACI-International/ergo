//! Script formatting.

use ergo_runtime::Source;
use ergo_script::ast::tokenize::{PairedToken, SymbolicToken, Token, Tokens};
use pretty::DocAllocator;
use std::borrow::Cow;
use std::io::{Read, Write};
use std::path::PathBuf;

#[derive(Debug, clap::Args)]
/// Format a script or scripts.
pub struct Format {
    #[clap(short)]
    /// Modify files in-place rather than printing to stdout.
    pub in_place: bool,
    /// Files to format. If no files are specified, stdin is read and the formatted script is written to stdout.
    pub files: Vec<PathBuf>,
}

impl Default for FormatOptions {
    fn default() -> Self {
        FormatOptions {
            line_width: 100,
            indent: 4,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FormatOptions {
    pub line_width: usize,
    pub indent: u8,
}

pub struct Formatter<'a> {
    pub options: FormatOptions,
    arena: pretty::Arena<'a>,
}

enum FormatTree<S> {
    Token(Token<S>),
    Nested {
        token: PairedToken,
        contents: Vec<FormatTree<S>>,
    },
}

impl<S> FormatTree<S> {
    pub fn from_tokens<E, I: Iterator<Item = Result<Token<S>, E>>>(
        tokens: &mut I,
    ) -> Result<Vec<Self>, E> {
        use Token::*;
        let mut ret = Vec::new();
        while let Some(t) = tokens.next() {
            match t? {
                EndNested => break,
                StartNested(pt) => ret.push(FormatTree::Nested {
                    token: pt,
                    contents: Self::from_tokens(tokens)?,
                }),
                other => ret.push(FormatTree::Token(other)),
            }
        }
        Ok(ret)
    }
}

#[derive(Clone, Debug)]
pub enum Error<E> {
    Error(E),
    Format(std::fmt::Error),
}

impl<E: std::fmt::Display> std::fmt::Display for Error<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::Error(e) => e.fmt(f),
            Error::Format(e) => e.fmt(f),
        }
    }
}

impl<E: std::error::Error + 'static> std::error::Error for Error<E> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::Error(e) => Some(e),
            Error::Format(e) => Some(e),
        }
    }
}

impl FormatOptions {
    pub fn format<
        'a,
        S: Clone + Into<Cow<'a, str>>,
        E,
        I: IntoIterator<Item = Result<Token<S>, E>>,
    >(
        &self,
        iter: I,
    ) -> Result<String, Error<E>> {
        let trees = FormatTree::from_tokens(&mut iter.into_iter()).map_err(Error::Error)?;
        let formatter = Formatter {
            options: *self,
            arena: pretty::Arena::new(),
        };
        let doc = formatter.children(&trees, formatter.arena.hardline()); // At the top-level, always use a line to separate children.
        let mut s = String::new();
        doc.render_fmt(self.line_width, &mut s)
            .map_err(Error::Format)?;
        Ok(s)
    }

    pub fn format_io<R, W, FR, FW, Name>(
        &self,
        read: FR,
        write: FW,
        name: Name,
    ) -> Result<(), String>
    where
        R: Read,
        W: Write,
        FR: FnOnce() -> std::io::Result<R>,
        FW: FnOnce() -> std::io::Result<W>,
        Name: std::fmt::Display,
    {
        let mut s = String::new();
        if let Err(e) = read().and_then(|mut r| r.read_to_string(&mut s)) {
            return Err(format!("failed to read {}: {}", name, e));
        }
        match self.format(
            Tokens::from(Source::new(0).with(s.as_str()))
                .map(|r| r.map(|s| s.unwrap()).map_err(|e| e.unwrap())),
        ) {
            Err(e) => Err(format!("failed to format {}: {}", name, e)),
            Ok(s) => {
                if let Err(e) = write().and_then(|mut w| write!(w, "{}", s)) {
                    Err(format!("failed to write {}: {}", name, e))
                } else {
                    Ok(())
                }
            }
        }
    }
}

impl<'a> Formatter<'a> {
    fn to_doc<'s: 'a, S: Clone + Into<Cow<'s, str>>>(
        &'a self,
        tree: &FormatTree<S>,
        in_quoted_string: bool,
    ) -> pretty::DocBuilder<'a, pretty::Arena<'a>> {
        use PairedToken::*;

        match tree {
            FormatTree::Token(t) => {
                use Token::*;
                match t {
                    String(s) => {
                        let mut s = s.clone().into();
                        if s.as_ref().ends_with('\n') {
                            match &mut s {
                                Cow::Borrowed(s) => *s = &s[0..s.len() - '\n'.len_utf8()],
                                Cow::Owned(s) => {
                                    s.pop();
                                }
                            }
                            self.arena.text(s).append(self.arena.hardline())
                        } else if s == "$" && in_quoted_string {
                            self.arena.text("$$")
                        } else {
                            self.arena.text(s)
                        }
                    }
                    Symbol(s) => self.arena.as_string(s),
                    Leader(s) => self.arena.as_string(s).append(self.arena.space()),
                    _ => panic!("unexpected token"),
                }
            }
            FormatTree::Nested { token, contents } => match token {
                Paren => self.enclose_group("(", ")", self.group(contents, self.arena.softline())),
                Curly => self.enclose_group(
                    "{",
                    "}",
                    self.children(
                        contents,
                        self.arena.hardline().flat_alt(self.arena.text(", ")),
                    ),
                ),
                Bracket => self.enclose_group(
                    "[",
                    "]",
                    self.children(
                        contents,
                        self.arena.hardline().flat_alt(self.arena.text(", ")),
                    ),
                ),
                Quote => self
                    .arena
                    .concat(contents.iter().map(|c| self.to_doc(c, true)))
                    .double_quotes(),
                Apostrophe | Hash | DoubleHash => self
                    .arena
                    .text(match token {
                        Apostrophe => "'",
                        Hash => "#",
                        DoubleHash => "##",
                        _ => panic!("unexpected paired token"),
                    })
                    .append(self.arena.space())
                    .append(self.arena.concat(contents.iter().map(|c| {
                        self.to_doc(
                            c,
                            match token {
                                Apostrophe | DoubleHash => true,
                                _ => false,
                            },
                        )
                    }))),
            },
        }
    }

    fn children<'s: 'a, S: Clone + Into<Cow<'s, str>>>(
        &'a self,
        trees: &[FormatTree<S>],
        sep: pretty::DocBuilder<'a, pretty::Arena<'a>>,
    ) -> pretty::DocBuilder<'a, pretty::Arena<'a>> {
        let mut groups = trees.split(|t| match t {
            FormatTree::Token(Token::NextChild) => true,
            _ => false,
        });

        let mut doc = self.arena.nil();
        let mut empty_lines = 0;
        let mut skip_sep = true;
        let mut first = true;
        while let Some(line) = groups.next() {
            if line.is_empty() {
                empty_lines += 1;
            } else {
                if !skip_sep {
                    doc += sep.clone();
                }
                if !first {
                    doc += self.arena.concat(
                        std::iter::repeat(self.arena.line_()).take(std::cmp::min(empty_lines, 2)),
                    );
                }
                doc += self.group(line, self.arena.space());
                empty_lines = 0;
                first = false;
                skip_sep = match line.first() {
                    // These always have a trailing hardline
                    Some(FormatTree::Nested {
                        token: PairedToken::Apostrophe | PairedToken::Hash | PairedToken::DoubleHash,
                        ..
                    }) => true,
                    _ => false,
                };
            }
        }
        doc
    }

    fn group<'s: 'a, S: Clone + Into<Cow<'s, str>>>(
        &'a self,
        trees: &[FormatTree<S>],
        sep: pretty::DocBuilder<'a, pretty::Arena<'a>>,
    ) -> pretty::DocBuilder<'a, pretty::Arena<'a>> {
        //let indent = trees.len() > 1;
        let mut trees = trees.iter();
        let mut doc = self.arena.nil();
        let mut skip_sep = true;
        while let Some(tree) = trees.next() {
            let mut should_skip_sep = false;
            match tree {
                // No whitespace before or after (infix)
                FormatTree::Token(Token::Symbol(
                    SymbolicToken::Colon | SymbolicToken::TildeEqual,
                )) => {
                    skip_sep = true;
                    should_skip_sep = true;
                }
                // No whitespace after (prefix)
                FormatTree::Token(Token::Symbol(
                    SymbolicToken::ColonPrefix
                    | SymbolicToken::Hash
                    | SymbolicToken::DoubleHash
                    | SymbolicToken::Tilde
                    | SymbolicToken::Caret
                    | SymbolicToken::Dollar
                    | SymbolicToken::DollarQuestion,
                )) => {
                    should_skip_sep = true;
                }
                _ => (),
            }
            if !skip_sep {
                doc = doc.append(sep.clone());
            }
            skip_sep = should_skip_sep;
            doc = doc.append(self.to_doc(tree, false));
        }
        /*
        doc.nest(if indent {
            self.options.indent as isize
        } else {
            0
        })
        */
        doc
    }

    fn enclose_group(
        &'a self,
        start: &'static str,
        end: &'static str,
        inner: pretty::DocBuilder<'a, pretty::Arena<'a>>,
    ) -> pretty::DocBuilder<'a, pretty::Arena<'a>> {
        self.arena
            .text(start)
            .append(self.arena.line_().append(inner).nest(4))
            .append(self.arena.line_())
            .append(self.arena.text(end))
            .group()
    }
}

impl super::Command for Format {
    fn run(self) -> Result<(), String> {
        let options = FormatOptions::default();

        if self.files.is_empty() {
            options.format_io(|| Ok(std::io::stdin()), || Ok(std::io::stdout()), "<stdin>")
        } else {
            for f in self.files {
                if self.in_place {
                    options.format_io(
                        || std::fs::File::open(&f),
                        || std::fs::File::create(&f),
                        f.display(),
                    )?;
                } else {
                    options.format_io(
                        || std::fs::File::open(&f),
                        || Ok(std::io::stdout()),
                        f.display(),
                    )?;
                }
            }
            Ok(())
        }
    }
}
