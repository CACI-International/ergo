//! The AST definition for script files.

use ergo_runtime::source::*;
use ergo_runtime::EvalResult;
use grease::depends;
use std::fmt;

mod parse;
mod tokenize;

/// A parsed expression.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
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
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    Force(Box<Expr>),
    DocComment(String, Box<Expr>),
    Compiled(CompiledExpression),
}

#[derive(Clone, Debug)]
pub struct CompiledExpression {
    pub value: EvalResult,
    pub from_env: bool,
}

/// A parsed pattern.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Pattern<Lit, Bnd> {
    Any,
    Literal(Lit),
    Binding(Bnd),
    Array(Vec<Source<ArrayPattern<Lit, Bnd>>>),
    Map(Vec<Source<MapPattern<Lit, Bnd>>>),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ArrayPattern<Lit, Bnd> {
    Item(PatT<Lit, Bnd>),
    Rest(PatT<Lit, Bnd>),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum MapPattern<Lit, Bnd> {
    Item(Lit, PatT<Lit, Bnd>),
    Rest(PatT<Lit, Bnd>),
}

pub type PatT<Lit, Bnd> = Source<Pattern<Lit, Bnd>>;

/// Parsed patterns with source information.
pub type Pat = PatT<Expr, Expr>;

/// A parsed merge expression.
///
/// A merge expression is an expression with a merge parameter. The parameter indicates whether the
/// contents of the value should be merged into the parent expression.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
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

/// Get Dependencies of the expression.
///
/// This takes into account the expression structure.
pub fn expr_dependencies(e: &Expr) -> grease::value::Dependencies {
    use Expression::*;
    let mut deps = depends![std::mem::discriminant(e)];

    pub fn pat_deps(pat: &Pat) -> grease::value::Dependencies {
        pattern_dependencies(pat, expr_dependencies, expr_dependencies)
    }

    match e.as_ref().unwrap() {
        Empty => (),
        String(s) => {
            deps += depends![s];
        }
        Array(es) => {
            for e in es {
                deps += depends![e.merge] + expr_dependencies(&e.expr);
            }
        }
        Set(pat, e) => {
            deps += pat_deps(pat) + expr_dependencies(e);
        }
        Unset(e) => {
            deps += expr_dependencies(e);
        }
        Get(e) => {
            deps += expr_dependencies(e);
        }
        Command(cmd, args) => {
            deps += expr_dependencies(cmd);
            for a in args {
                deps += depends![a.merge] + expr_dependencies(&a.expr);
            }
        }
        Block(es) => {
            for e in es {
                deps += depends![e.merge] + expr_dependencies(&e.expr);
            }
        }
        Function(pat, e) => {
            for p in pat.as_ref().unwrap() {
                let p = p.as_ref().unwrap();
                deps += depends![std::mem::discriminant(p)];
                match p {
                    ArrayPattern::Item(p) => deps += pat_deps(p),
                    ArrayPattern::Rest(p) => deps += pat_deps(p),
                }
            }

            deps += expr_dependencies(e);
        }
        Match(e, pats) => {
            deps += expr_dependencies(e);
            for (p, e) in pats {
                deps += pat_deps(p) + expr_dependencies(e);
            }
        }
        If(cond, t, f) => {
            deps += expr_dependencies(cond)
                + expr_dependencies(t)
                + depends![std::mem::discriminant(f)];
            if let Some(f) = f {
                deps += expr_dependencies(f);
            }
        }
        Force(e) => {
            deps += expr_dependencies(e);
        }
        DocComment(_, e) => {
            deps += expr_dependencies(e);
        }
        Compiled(CompiledExpression { value, .. }) => match value {
            Ok(v) => deps += depends![**v],
            Err(_) => (),
        },
    }

    deps
}

/// Get Dependencies of the pattern.
///
/// This takes into account the pattern structure.
pub fn pattern_dependencies<L, B, LF, BF>(
    pat: &PatT<L, B>,
    literal_deps: LF,
    binding_deps: BF,
) -> grease::value::Dependencies
where
    LF: Fn(&L) -> grease::value::Dependencies + Copy,
    BF: Fn(&B) -> grease::value::Dependencies + Copy,
{
    use Pattern::*;
    let mut deps = depends![std::mem::discriminant(pat)];
    match pat.as_ref().unwrap() {
        Any => (),
        Literal(l) => deps += literal_deps(l),
        Binding(b) => deps += binding_deps(b),
        Array(pats) => {
            for p in pats {
                let p = p.as_ref().unwrap();
                deps += depends![std::mem::discriminant(p)];
                match p {
                    ArrayPattern::Item(p) => {
                        deps += pattern_dependencies(p, literal_deps, binding_deps)
                    }
                    ArrayPattern::Rest(p) => {
                        deps += pattern_dependencies(p, literal_deps, binding_deps)
                    }
                }
            }
        }
        Map(pats) => {
            for p in pats {
                let p = p.as_ref().unwrap();
                deps += depends![std::mem::discriminant(p)];
                match p {
                    MapPattern::Item(_, p) => {
                        deps += pattern_dependencies(p, literal_deps, binding_deps)
                    }
                    MapPattern::Rest(p) => {
                        deps += pattern_dependencies(p, literal_deps, binding_deps)
                    }
                }
            }
        }
    }
    deps
}

impl Expression {
    /// Return whether the expression is a compiled expression.
    pub fn is_compiled_from_env(&self) -> bool {
        match self {
            Expression::Compiled(e) => e.from_env,
            _ => false,
        }
    }
}

impl Ord for CompiledExpression {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (&self.value, &other.value) {
            (Ok(a), Ok(b)) => a.cmp(b),
            (Ok(_), Err(_)) => std::cmp::Ordering::Less,
            (Err(_), Ok(_)) => std::cmp::Ordering::Greater,
            (Err(_), Err(_)) => std::cmp::Ordering::Equal,
        }
    }
}

impl PartialOrd for CompiledExpression {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for CompiledExpression {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == std::cmp::Ordering::Equal
    }
}

impl Eq for CompiledExpression {}

impl CompiledExpression {
    pub fn new(value: EvalResult, from_env: bool) -> Self {
        CompiledExpression { value, from_env }
    }
}

impl From<EvalResult> for CompiledExpression {
    fn from(value: EvalResult) -> Self {
        Self::new(value, false)
    }
}

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
