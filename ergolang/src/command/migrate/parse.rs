//! AST parsing.
//!
//! The parser takes an iterator of parsed `parse_tree::Tree`s and produces an iterator of
//! top-level `BlockItem` expressions. In doing so some syntax is desugared, such as bare strings
//! implying get, set, or bind operations (depending on context). Commands and pattern commands are
//! also disambiguated depending on context.

use super::parse_tree::{StringTree, Tree};
use super::Migration;
use ergo_runtime::Source;
use std::fmt;

pub(super) type Migrations = Vec<Migration>;

pub(super) fn migrations<TreeIter, E>(
    mut iter: TreeIter,
) -> Result<Migrations, Vec<Source<Error<E>>>>
where
    TreeIter: Iterator<Item = Result<Source<Tree>, Vec<Source<E>>>>,
{
    let mut results = Migrations::new();
    while let Some(v) = to_block_item(&mut results, Default::default(), &mut iter, false) {
        v?;
    }
    Ok(results)
}

struct NoCommentsResult<T>(T);

impl<T, E> Iterator for NoCommentsResult<T>
where
    T: Iterator<Item = Result<Source<Tree>, Vec<Source<E>>>>,
{
    type Item = T::Item;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.0.next() {
                None => break None,
                Some(Err(e)) => break Some(Err(e)),
                Some(Ok(t)) => match t.value() {
                    Tree::Hash(_) | Tree::HashSpace(_) => continue,
                    _ => break Some(Ok(t)),
                },
            }
        }
    }
}

struct NoComments<T>(T);

fn first(mut s: Source<()>) -> Source<()> {
    s.location.length = 1;
    s
}

impl<T> Iterator for NoComments<T>
where
    T: Iterator<Item = Source<Tree>>,
{
    type Item = T::Item;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.0.next() {
                None => break None,
                Some(t) => match t.value() {
                    Tree::Hash(_) | Tree::HashSpace(_) => continue,
                    _ => break Some(t),
                },
            }
        }
    }
}

/// Parsing errors.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error<TreeError> {
    /// A tree parsing error.
    TreeParse(TreeError),
    /// A caret operator was in an invalid position.
    BadCaret,
    /// An equal operator was in an invalid position.
    BadEqual,
    /// An attribute was in an invalid position.
    BadAttribute,
    /// A comment was in an invalid position.
    BadComment,
}

impl<E: fmt::Display> fmt::Display for Error<E> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::TreeParse(t) => t.fmt(f),
            Error::BadCaret => write!(f, "cannot merge values here"),
            Error::BadEqual => write!(f, "cannot bind values here"),
            Error::BadAttribute => write!(
                f,
                "attributes and doc comments can only be within blocks or brackets preceding an expression or binding, or directly preceding a single expression"
            ),
            Error::BadComment => write!(f, "comments are not valid in this context"),
        }
    }
}

impl<E: std::error::Error + 'static> std::error::Error for Error<E> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::TreeParse(t) => Some(t),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum StringImplies {
    Get,
    Set,
    None,
}

#[derive(Clone, Copy, Debug)]
struct ParseContext {
    pub pattern: bool,
    pub string_implies: StringImplies,
}

impl Default for ParseContext {
    fn default() -> Self {
        ParseContext {
            pattern: false,
            string_implies: StringImplies::None,
        }
    }
}

impl ParseContext {
    pub fn pattern(&self, pattern: bool) -> Self {
        ParseContext { pattern, ..*self }
    }

    pub fn string_implies(&self, string_implies: StringImplies) -> Self {
        ParseContext {
            string_implies,
            ..*self
        }
    }
}

/// Parse an expression optionally preceded by attributes and doc comments.
fn attributed<I: Iterator<Item = Result<Source<Tree>, Vec<Source<E>>>>, E, Parse, SubExpr, R>(
    migrations: &mut Migrations,
    ctx: ParseContext,
    iter: &mut I,
    parse: Parse,
    sub_expr: SubExpr,
) -> Option<Result<R, Vec<Source<Error<E>>>>>
where
    Parse: FnOnce(&mut Migrations, ParseContext, Source<Tree>) -> Result<R, Vec<Source<Error<E>>>>,
    SubExpr: for<'a> FnOnce(&'a mut R, &mut I) -> bool,
{
    type Attr = ();
    let mut attributes: Vec<Source<Attr>> = Vec::new();

    let mut iter = NoCommentsResult(iter);

    fn invalid_items<E>(attributes: Vec<Source<Attr>>) -> Vec<Source<Error<E>>> {
        attributes
            .into_iter()
            .map(|attr| attr.with(Error::BadAttribute))
            .collect()
    }

    // TODO catch more than one error?
    loop {
        match iter.next() {
            None => {
                if attributes.is_empty() {
                    break None;
                } else {
                    break Some(Err(invalid_items(attributes)));
                }
            }
            Some(Err(errs)) => {
                break Some(Err(errs
                    .into_iter()
                    .map(|e| e.map(Error::TreeParse))
                    .collect()))
            }
            Some(Ok(t)) => {
                let (source, t) = t.take();
                match t {
                    Tree::DoubleHashSpace(parts) => match string_parts(migrations, ctx, parts) {
                        Ok(()) => attributes.push(source),
                        Err(e) => break Some(Err(e)),
                    },
                    Tree::DoubleHash(inner) => {
                        match to_expression(
                            migrations,
                            ctx.pattern(false).string_implies(StringImplies::Get),
                            *inner,
                        ) {
                            Ok(()) => attributes.push(source),
                            Err(e) => break Some(Err(e)),
                        }
                    }
                    o => {
                        let mut v = match parse(migrations, ctx, source.with(o)) {
                            Err(e) => break Some(Err(e)),
                            Ok(v) => v,
                        };
                        if !attributes.is_empty() {
                            if sub_expr(&mut v, iter.0) {
                                while let Some(_attr) = attributes.pop() {}
                            } else {
                                break Some(Err(invalid_items(attributes)));
                            }
                        }
                        break Some(Ok(v));
                    }
                }
            }
        }
    }
}

fn string_parts<E>(
    migrations: &mut Migrations,
    ctx: ParseContext,
    trees: Vec<Source<StringTree>>,
) -> Result<(), Vec<Source<Error<E>>>> {
    // TODO catch more than one error (don't use collect())?
    trees
        .into_iter()
        .map(|p| {
            let (source, p) = p.take();
            Ok(match p {
                StringTree::String(s) => {
                    if s == "^" {
                        migrations.push(Migration::syntax(
                            source,
                            "^ is no longer a special symbol in quoted strings",
                            Some("^"),
                        ));
                    } else if s.contains('$') {
                        for (i, _) in s.match_indices('$') {
                            migrations.push(Migration::syntax(
                                {
                                    let mut s = source;
                                    s.location.start += i;
                                    s.location.length = 1;
                                    s
                                },
                                "a literal `$` in quoted strings must be escaped as `$$`",
                                Some("$$"),
                            ));
                        }
                    }
                }
                StringTree::Expression(tree) => {
                    migrations.push(Migration::syntax(
                        first(source),
                        "string interpolation is now done with `$`",
                        Some("$"),
                    ));
                    to_expression(
                        migrations,
                        ctx.pattern(false).string_implies(StringImplies::Get),
                        tree,
                    )?;
                }
            })
        })
        .collect()
}

fn to_basic_block_item<E>(
    migrations: &mut Migrations,
    ctx: ParseContext,
    t: Source<Tree>,
    string_implies_bind: bool,
) -> Result<&'static str, Vec<Source<Error<E>>>> {
    let (source, t) = t.take();

    match t {
        // string_implies should only affect direct descendants, set to None for caret and equal
        Tree::Caret(t) => {
            let (t_source, t) = t.take();
            match &t {
                Tree::String(s) => {
                    migrations.push(Migration::syntax(
                        source,
                        "you can no longer merge strings; use `~` to set keyed values",
                        Some(format!("~{}", s)),
                    ));
                }
                _ => (),
            }
            to_expression(
                migrations,
                ctx.string_implies(StringImplies::None),
                t_source.with(t),
            )?;
            Ok("merge")
        }
        Tree::Equal(a, b) => {
            to_expression(
                migrations,
                ctx.pattern(true).string_implies(StringImplies::Set),
                *a,
            )?;
            to_expression(migrations, ctx.string_implies(StringImplies::None), *b)?;
            Ok("bind")
        }
        // elaborate an unquoted string literal `a` to `:a = :a`
        Tree::String(s) if string_implies_bind => {
            if ctx.pattern {
                migrations.push(Migration::syntax(
                    source,
                    "pattern map setter syntax sugar now requires a `:`",
                    Some(format!(":{}", s)),
                ));
            }
            Ok("bind")
        }
        t => {
            to_expression(migrations, ctx, source.with(t))?;
            Ok("expr")
        }
    }
}

fn to_block_item<I: Iterator<Item = Result<Source<Tree>, Vec<Source<E>>>>, E>(
    migrations: &mut Migrations,
    ctx: ParseContext,
    iter: &mut I,
    string_implies_bind: bool,
) -> Option<Result<(), Vec<Source<Error<E>>>>> {
    attributed(
        migrations,
        ctx,
        iter,
        |migrations, ctx, t| to_basic_block_item(migrations, ctx, t, string_implies_bind),
        |block_item, _| match *block_item {
            "expr" | "bind" => true,
            _ => false,
        },
    )
    .map(|res| res.map(|_| ()))
}

fn to_basic_array_item<E>(
    migrations: &mut Migrations,
    ctx: ParseContext,
    t: Source<Tree>,
) -> Result<&'static str, Vec<Source<Error<E>>>> {
    let (source, t) = t.take();

    match t {
        // string_implies should only affect direct descendants, set to None for caret
        Tree::Caret(t) => {
            to_expression(migrations, ctx.string_implies(StringImplies::None), *t)?;
            Ok("merge")
        }
        t => {
            to_expression(migrations, ctx, source.with(t))?;
            Ok("expr")
        }
    }
}

fn to_array_item<I: Iterator<Item = Result<Source<Tree>, Vec<Source<E>>>>, E>(
    migrations: &mut Migrations,
    ctx: ParseContext,
    iter: &mut I,
) -> Option<Result<(), Vec<Source<Error<E>>>>> {
    attributed(
        migrations,
        ctx,
        iter,
        to_basic_array_item,
        |array_item, _| match *array_item {
            "expr" => true,
            _ => false,
        },
    )
    .map(|res| res.map(|_| ()))
}

fn to_expression<E>(
    migrations: &mut Migrations,
    mut ctx: ParseContext,
    t: Source<Tree>,
) -> Result<(), Vec<Source<Error<E>>>> {
    let (source, t) = t.take();
    // string_implies should only affect direct descendants
    let _string_implies = ctx.string_implies;
    ctx = ctx.string_implies(StringImplies::None);
    match t {
        Tree::String(_) => Ok(()),
        Tree::Bang(t) => {
            if ctx.pattern {
                // Bang in a pattern interprets as a non-pattern expression
                migrations.push(Migration::syntax(
                    first(source),
                    "pattern and normal expression contexts no longer exist (so no need for `!`)",
                    Some(""),
                ));
                to_expression(migrations, ctx.pattern(false), *t)?;
            } else {
                migrations.push(Migration::semantics(
                    first(source),
                    "the force operator no longer exists (ensure new code works as expected)",
                    "",
                ));
                to_expression(migrations, ctx, *t)?;
            }
            Ok(())
        }
        Tree::Caret(_) => {
            // Caret not allowed in expressions
            Err(vec![source.with(Error::BadCaret)])
        }
        Tree::ColonPrefix(t) => {
            if ctx.pattern {
                to_expression(migrations, ctx.pattern(false), *t)?;
                Ok(())
            } else {
                match t.value() {
                    Tree::String(_) => {
                        migrations.push(Migration::syntax(
                            first(source),
                            "the get operator is now `$`",
                            Some("$"),
                        ));
                        to_expression(migrations, ctx, *t)?;
                    }
                    _ => {
                        migrations.push(Migration::semantics_warn(
                            t.source(),
                            "get may only be used with string literals; use indexing or explicit matching",
                        ));
                    }
                }
                Ok(())
            }
        }
        Tree::Equal(_, _) => Err(vec![source.with(Error::BadEqual)]),
        Tree::Arrow(a, b) => {
            to_expression(migrations, ctx.pattern(true), *a)?;
            to_expression(migrations, ctx.pattern(false), *b)?;
            Ok(())
        }
        Tree::Colon(a, b) => {
            to_expression(migrations, ctx.string_implies(StringImplies::Get), *a)?;
            to_expression(migrations, ctx, *b)?;
            Ok(())
        }
        Tree::Parens(inner) => {
            let args = inner.len();
            let mut inner = NoComments(inner.into_iter()).peekable();
            match inner.peek() {
                None => Ok(()),
                Some(f) => {
                    // See if the first item is an attribute, and parse accordingly.
                    match f.value() {
                        Tree::DoubleHash(_) | Tree::DoubleHashSpace(_) => {
                            attributed(
                                migrations,
                                ctx,
                                &mut (&mut inner).map(Ok).peekable(),
                                to_expression,
                                |_, inner| {
                                    if inner.peek().is_none() {
                                        true
                                    } else {
                                        // Attributes cannot be applied if there are multiple values
                                        // following.
                                        false
                                    }
                                },
                            )
                            .unwrap()
                            .map(|_| ())
                        }
                        _ => {
                            let f = inner.next().unwrap();
                            if inner.peek().is_none() {
                                to_expression(migrations, ctx, f)
                            } else {
                                if let Tree::String(k) = f.value() {
                                    if k == "pat" {
                                        migrations.push(Migration::syntax(
                                            f.source(),
                                            "`pat` has been removed; use `fn`",
                                            Some("fn"),
                                        ));
                                    } else if k == "index" && args == 3 {
                                        migrations.push(Migration::semantics_warn(
                                            source,
                                            "`index a b` has been removed; use `:`",
                                        ));
                                    }
                                }
                                to_expression(
                                    migrations,
                                    ctx.pattern(false).string_implies(StringImplies::Get),
                                    f,
                                )?;
                                inner
                                    .map(|v| to_basic_block_item(migrations, ctx, v, false))
                                    .collect::<Result<Vec<_>, _>>()?;
                                Ok(())
                            }
                        }
                    }
                }
            }
        }
        Tree::Curly(inner) => {
            let mut inner = inner.into_iter().map(Ok);
            std::iter::from_fn(move || to_block_item(migrations, ctx, &mut inner, true))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(())
        }
        Tree::Bracket(inner) => {
            let mut inner = inner.into_iter().map(Ok);
            std::iter::from_fn(move || to_array_item(migrations, ctx, &mut inner))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(())
        }
        Tree::Quote(strings) | Tree::ApostropheSpace(strings) => {
            string_parts(migrations, ctx, strings)?;
            Ok(())
        }
        Tree::Hash(_) | Tree::HashSpace(_) => Err(vec![source.with(Error::BadComment)]),
        Tree::DoubleHash(_) | Tree::DoubleHashSpace(_) => {
            Err(vec![source.with(Error::BadAttribute)])
        }
    }
}
