//! The AST definition for script files.

use ergo_runtime::source::*;
use std::fmt;

mod parse;
mod tokenize;

/// A parsed expression.
#[derive(Clone, Debug, Hash, PartialEq)]
pub enum Expression {
    Empty,
    String(String),
    Array(Exprs),
    Set(Box<Pat>, Box<Expr>),
    Unset(Box<Expr>),
    Get(Box<Expr>),
    Command(Box<Expr>, Exprs),
    Block(Exprs),
    Function(CmdPat, Box<Expr>),
    Match(Box<Expr>, Vec<(Pat, Expr)>),
}

/// A parsed pattern.
#[derive(Clone, Debug, Hash, PartialEq)]
pub enum Pattern<Lit, Bnd> {
    Any,
    Literal(Lit),
    Binding(Bnd),
    Array(Vec<Source<ArrayPattern<Lit, Bnd>>>),
    Map(Vec<Source<MapPattern<Lit, Bnd>>>),
}

#[derive(Clone, Debug, Hash, PartialEq)]
pub enum ArrayPattern<Lit, Bnd> {
    Item(PatT<Lit, Bnd>),
    Rest(PatT<Lit, Bnd>),
}

#[derive(Clone, Debug, Hash, PartialEq)]
pub enum MapPattern<Lit, Bnd> {
    Item(Bnd, PatT<Lit, Bnd>),
    Rest(PatT<Lit, Bnd>),
}

pub type PatT<Lit, Bnd> = Source<Pattern<Lit, Bnd>>;

/// Parsed patterns with source information.
pub type Pat = PatT<Expr, Expr>;

/// A parsed merge expression.
///
/// A merge expression is an expression with a merge parameter. The parameter indicates whether the
/// contents of the value should be merged into the parent expression.
#[derive(Clone, Debug, Hash, PartialEq)]
pub struct MergeExpression {
    pub merge: bool,
    pub expr: Expr,
}

/// Expressions with source information.
pub type Expr = Source<Expression>;

/// A merge expression with source information.
pub type MergeExpr = Source<MergeExpression>;

pub type CmdPatT<Lit, Bnd> = Source<Vec<Source<ArrayPattern<Lit, Bnd>>>>;

/// A parsed command pattern.
///
/// This is of primary use in function definitions.
pub type CmdPat = CmdPatT<Expr, Expr>;

/// Multiple expressions.
///
/// Anything that accepts multiple expressions should also support merging.
pub type Exprs = Vec<MergeExpr>;

/// A parsed script.
pub type Script = Source<Exprs>;

/// A script loading error.
#[derive(Debug)]
pub enum Error {
    Io(std::io::Error),
    /// A tokenization error.
    Tokenize(Source<tokenize::Error>),
    /// A parsing error.
    Parse(Source<pom::Error>),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Io(e) => write!(f, "io error: {}", e),
            Error::Tokenize(e) => write!(f, "syntax error: {}", e),
            Error::Parse(e) => write!(f, "syntax error: {}", e),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(match self {
            Error::Io(e) => e,
            Error::Tokenize(e) => e,
            Error::Parse(e) => e,
        })
    }
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error::Io(e)
    }
}

impl From<Source<tokenize::Error>> for Error {
    fn from(e: Source<tokenize::Error>) -> Self {
        Error::Tokenize(e)
    }
}

impl From<Source<pom::Error>> for Error {
    fn from(e: Source<pom::Error>) -> Self {
        Error::Parse(e)
    }
}

/// Load an AST from the given character stream.
pub fn load(src: Source<()>) -> Result<Script, Error> {
    let toks = tokenize::Tokens::from(src.clone().open()?).collect::<Result<Vec<_>, _>>()?;
    let parser = parse::script();
    let parse_error = |e: pom::Error, pos: Option<usize>| {
        pos.and_then(|pos| toks.get(pos))
            .map(|p| p.clone().with(()))
            .unwrap_or(src)
            .with(e)
    };
    parser.parse(&toks).map_err(move |e| {
        Error::Parse(match e {
            pom::Error::Incomplete => parse_error(e, None),
            pom::Error::Mismatch { position, .. } => parse_error(e, Some(position)),
            pom::Error::Conversion { position, .. } => parse_error(e, Some(position)),
            pom::Error::Expect { position, .. } => parse_error(e, Some(position)),
            pom::Error::Custom { position, .. } => parse_error(e, Some(position)),
        })
    })
}
