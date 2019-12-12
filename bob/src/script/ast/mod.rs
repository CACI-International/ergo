//! The AST definition for script files.

use std::fmt;

mod parse;
mod tokenize;

/// A parsed expression.
#[derive(Clone, Debug)]
pub enum Expression {
    String(String),
    Array(Vec<Expression>),
    SetVariable(String, Box<Expression>),
    UnsetVariable(String),
    Index(Box<Expression>, String),
    Command(Box<Expression>, Vec<Expression>),
    Block(Vec<Expression>),
    Function(Box<Expression>),
    If(Box<Expression>, Box<Expression>, Box<Expression>),
}

/// A location in the original (character) input stream.
#[derive(Clone, Debug, PartialEq)]
pub struct Location {
    pub start: usize,
    pub length: usize,
}

impl Location {
    /// Create a Location with the given fields.
    pub fn new(start: usize, length: usize) -> Self {
        Location { start, length }
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}-{}", self.start, self.start + self.length - 1)
    }
}

/// A parsed script.
pub type Script = Vec<Expression>;

/// A script loading error.
pub enum Error {
    /// A tokenization error.
    Tokenize(tokenize::Error, Location),
    /// A parsing error.
    Parse(pom::Error, Option<Location>),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Tokenize(e, l) => write!(f, "{} (at {})", e, l),
            Self::Parse(e, l) => {
                write!(f, "{}", e)?;
                if let Some(loc) = l {
                    write!(f, " (at {})", loc)?;
                }
                Ok(())
            }
        }
    }
}

/// Load an AST from the given character stream.
pub fn load<I: Iterator<Item = char>>(i: I) -> Result<Script, Error> {
    let (toks, locs): (Vec<_>, Vec<_>) = tokenize::Tokens::new(i)
        .collect::<Result<Vec<_>, _>>()
        .map_err(|(e, l)| Error::Tokenize(e, l))?
        .into_iter()
        .unzip();
    let parser = parse::script();
    let parse_error = move |e: pom::Error, pos: usize| Error::Parse(e, locs.get(pos).cloned());
    parser.parse(&toks).map_err(move |e| match e {
        pom::Error::Incomplete => Error::Parse(e, None),
        pom::Error::Mismatch { position, .. } => parse_error(e, position),
        pom::Error::Conversion { position, .. } => parse_error(e, position),
        pom::Error::Expect { position, .. } => parse_error(e, position),
        pom::Error::Custom { position, .. } => parse_error(e, position),
    })
}
