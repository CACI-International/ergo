//! The AST definition for script files.

use ergo_runtime::source::*;
use ergo_runtime::ResultIterator;
use grease::{depends, Value};
use std::fmt;

mod parse;
mod parse_tree;
mod tokenize;
mod tokenize_tree;

/// A parsed expression.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expression {
    Empty,
    BindAny,
    String(String),
    Array(Vec<Expr>),
    Map(Vec<Expr>),
    Block(Vec<Expr>),
    Function(Box<Expr>, Box<Expr>),
    Bind(Box<Expr>, Box<Expr>),
    Get(Box<Expr>),
    Set(Box<Expr>),
    BindEqual(Box<Expr>, Box<Expr>),
    Index(Box<Expr>, Box<Expr>),
    Command(Box<Expr>, Vec<Expr>),
    BindCommand(Box<Expr>, Vec<Expr>),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    IfBind(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    Force(Box<Expr>),
    Merge(Box<Expr>),
    DocComment(String, Box<Expr>),
    Compiled(Value),
}

/// Expressions with source information.
pub type Expr = Source<Expression>;

impl From<&Expression> for grease::value::Dependencies {
    fn from(e: &Expression) -> Self {
        use Expression::*;
        let mut deps = depends![std::mem::discriminant(e)];

        match e {
            Empty | BindAny => (),
            String(s) => {
                deps += depends![s];
            }
            Get(e) | Set(e) | Force(e) | Merge(e) | DocComment(_, e) => {
                deps += Self::from(&***e);
            }
            Array(es) | Map(es) | Block(es) => {
                for e in es {
                    deps += Self::from(&**e);
                }
            }
            Function(a, b) | Bind(a, b) | BindEqual(a, b) | Index(a, b) => {
                deps += Self::from(&***a) + Self::from(&***b);
            }
            Command(cmd, args) | BindCommand(cmd, args) => {
                deps += Self::from(&***cmd);
                for a in args {
                    deps += Self::from(&**a);
                }
            }
            If(cond, t, f) | IfBind(cond, t, f) => {
                deps +=
                    Self::from(&***cond) + Self::from(&***t) + depends![std::mem::discriminant(f)];
                if let Some(f) = f {
                    deps += Self::from(&***f);
                }
            }
            Compiled(v) => deps += depends![*v],
        }

        deps
    }
}

impl Expression {
    /// Create a Map or Block from a set of expressions based on the final expression.
    pub fn map_or_block(exprs: Vec<Expr>) -> Self {
        // If the last expression is a Bind or Merge expression (or there are no expressions), create a
        // Map. Otherwise create a Block.
        if exprs
            .last()
            .map(|v| match &**v {
                Expression::Bind(_, _) | Expression::Merge(_) => true,
                _ => false,
            })
            .unwrap_or(true)
        {
            Expression::Map(exprs)
        } else {
            Expression::Block(exprs)
        }
    }
}

/// Load an AST from the given character stream.
pub fn load(src: Source<()>) -> Result<Expr, grease::Error> {
    let toks = tokenize::Tokens::from(src.open()?);
    let tree_toks = tokenize_tree::TreeTokens::from(toks);
    let tree_parser = parse_tree::Parser::from(tree_toks);
    let parser = parse::Parser::from(tree_parser);

    parser
        .map(|v| v.map_err(|e| grease::Error::from(e)))
        .collect_result::<Vec<_>>()
        .map(|vec| vec.into_source().map(Expression::map_or_block))
}
