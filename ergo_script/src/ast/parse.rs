//! AST parsing.

use super::parse_tree::Tree;
use super::*;
use ergo_runtime::Source;

/// A parser for Expressions.
///
/// This is used as an iterator, producing Expr as individual items in a script.
pub struct Parser<TreeIter> {
    trees: TreeIter,
    ctx: ParseContext,
}

impl<T, E> From<T> for Parser<T::IntoIter>
where
    T: IntoIterator<Item = Result<Source<Tree>, Source<E>>>,
{
    fn from(iter: T) -> Self {
        Parser {
            trees: iter.into_iter(),
            ctx: Default::default(),
        }
    }
}

impl<I> Parser<I> {
    fn with_context(trees: I, ctx: ParseContext) -> Self {
        Parser { trees, ctx }
    }
}

impl<I, E> Iterator for Parser<I>
where
    I: Iterator<Item = Result<Source<Tree>, Source<E>>>,
{
    type Item = Result<Source<Expression>, Source<Error<E>>>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut doc_comment_lines = Vec::new();
        loop {
            match self.trees.next() {
                None => break None,
                Some(Err(e)) => break Some(Err(e.map(Error::TreeParse))),
                Some(Ok(t)) => {
                    let (source, t) = t.take();
                    match t {
                        Tree::DocComment(s) => {
                            doc_comment_lines.push(s);
                        }
                        o => {
                            let e = match to_expression(self.ctx, source.with(o)) {
                                Ok(v) => v,
                                Err(e) => break Some(Err(e)),
                            };
                            let doc_comment = {
                                let mut doc_comment_lines = doc_comment_lines.into_iter();
                                let mut doc_comment = String::new();

                                // Use whitespace of first line of doc comment to determine trimmable
                                // whitespace from subsequent lines.
                                let mut leading_ws_offset = 0;
                                while let Some(line) = doc_comment_lines.next() {
                                    match line.find(|c: char| !c.is_whitespace()) {
                                        None => continue,
                                        Some(offset) => {
                                            leading_ws_offset = offset;
                                            doc_comment
                                                .push_str(unsafe { line.get_unchecked(offset..) });
                                            break;
                                        }
                                    }
                                }

                                // Get remaining lines and trim leading whitespace if necessary.
                                while let Some(line) = doc_comment_lines.next() {
                                    doc_comment.push('\n');
                                    let offset = std::cmp::min(
                                        leading_ws_offset,
                                        line.find(|c: char| !c.is_whitespace()).unwrap_or(0),
                                    );
                                    doc_comment.push_str(unsafe { line.get_unchecked(offset..) });
                                }

                                // We already have done the equivalent of trim_start, so just need
                                // to trim_end.
                                doc_comment.trim_end().to_owned()
                            };
                            break Some(Ok(if !doc_comment.is_empty() {
                                // If a doc comment precedes a bind expression, apply it to the value.
                                // Otherwise apply it to whatever value it precedes.
                                let (expr_source, expr) = e.take();
                                match expr {
                                    Expression::Bind(a, b) => expr_source.with(Expression::Bind(
                                        a,
                                        b.source()
                                            .with(Expression::DocComment(doc_comment, b))
                                            .into(),
                                    )),
                                    o => expr_source.clone().with(Expression::DocComment(
                                        doc_comment,
                                        expr_source.with(o).into(),
                                    )),
                                }
                            } else {
                                e
                            }));
                        }
                    }
                }
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
    /// An if expression has an invalid number of arguments.
    MalformedIf,
    /// A doc comment was in an invalid position.
    BadDocComment,
}

impl<E: fmt::Display> fmt::Display for Error<E> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::TreeParse(t) => write!(f, "{}", t),
            Error::BadCaret => write!(f, "cannot merge values here"),
            Error::MalformedIf => write!(f, "if expression must have 2 or 3 arguments"),
            Error::BadDocComment => write!(
                f,
                "doc comments can only be at the top-level or within blocks or brackets"
            ),
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
struct ParseContext {
    pub allow_merge: bool,
    pub binding: bool,
    pub bind_equal: bool,
    pub string_to_set: bool,
}

impl Default for ParseContext {
    fn default() -> Self {
        ParseContext {
            allow_merge: true,
            binding: false,
            bind_equal: false,
            string_to_set: false,
        }
    }
}

impl ParseContext {
    pub fn allow_merge(&self, allow_merge: bool) -> Self {
        ParseContext {
            allow_merge,
            ..*self
        }
    }

    // Implies bind_equal
    pub fn binding(&self, binding: bool) -> Self {
        ParseContext {
            binding,
            bind_equal: binding,
            ..*self
        }
    }

    pub fn bind_equal(&self, bind_equal: bool) -> Self {
        ParseContext {
            bind_equal,
            ..*self
        }
    }

    pub fn string_to_set(&self, string_to_set: bool) -> Self {
        ParseContext {
            string_to_set,
            ..*self
        }
    }
}

fn to_expression<E>(
    mut ctx: ParseContext,
    t: Source<Tree>,
) -> Result<Source<Expression>, Source<Error<E>>> {
    let (source, t) = t.take();
    // string_to_set should only affect direct descendants
    let string_to_set = ctx.string_to_set;
    ctx = ctx.string_to_set(false);
    match t {
        Tree::String(s) => {
            if ctx.binding && s == "_" {
                Ok(source.with(Expression::BindAny))
            } else if string_to_set {
                Ok(source
                    .clone()
                    .with(Expression::Set(source.with(Expression::String(s)).into())))
            } else {
                Ok(source.with(Expression::String(s)))
            }
        }
        Tree::Bang(t) => {
            if ctx.binding {
                // bang equal will only change bind equality
                let ctx = if let Tree::Equal(_, _) = &**t {
                    ctx.bind_equal(false)
                } else {
                    ctx.binding(false)
                };
                to_expression(ctx, *t)
            } else {
                Ok(source.with(Expression::Force(
                    to_expression(ctx.allow_merge(false), *t)?.into(),
                )))
            }
        }
        Tree::Caret(t) => {
            if ctx.allow_merge {
                let r = to_expression(ctx.allow_merge(false), *t)?;
                Ok(source.with(Expression::Merge(r.into())))
            } else {
                Err(source.with(Error::BadCaret))
            }
        }
        Tree::Colon(t) => {
            let ctx = ctx.allow_merge(false);
            if ctx.binding {
                Ok(source.with(Expression::Set(
                    to_expression(ctx.binding(false), *t)?.into(),
                )))
            } else {
                Ok(source.with(Expression::Get(to_expression(ctx, *t)?.into())))
            }
        }
        Tree::Equal(a, b) => {
            if ctx.bind_equal {
                Ok(source.with(Expression::BindEqual(
                    to_expression(ctx, *a)?.into(),
                    to_expression(ctx, *b)?.into(),
                )))
            } else {
                let ctx = ctx.allow_merge(false);
                Ok(source.with(Expression::Bind(
                    to_expression(ctx.binding(true).string_to_set(true), *a)?.into(),
                    to_expression(ctx.bind_equal(ctx.binding), *b)?.into(),
                )))
            }
        }
        Tree::Arrow(a, b) => {
            let ctx = ctx.allow_merge(false);
            Ok(source.with(Expression::Function(
                to_expression(ctx.binding(true), *a)?.into(),
                to_expression(ctx.binding(false), *b)?.into(),
            )))
        }
        Tree::Parens(inner) => {
            if inner.len() == 0 {
                Ok(source.with(Expression::Empty))
            } else {
                let mut inner = inner.into_iter();
                let f = inner.next().unwrap();
                let is_if = if let Tree::String(s) = &*f {
                    s == "if"
                } else {
                    false
                };
                if is_if {
                    let ctx = ctx.allow_merge(false);
                    let cond = to_expression(
                        ctx,
                        match inner.next() {
                            Some(v) => v,
                            None => return Err(source.with(Error::MalformedIf)),
                        },
                    )?;
                    // If cond is a Bind expression, change this to an IfBind
                    let is_if_bind = if let Expression::Bind(_, _) = &*cond {
                        true
                    } else {
                        false
                    };
                    let if_true = to_expression(
                        ctx,
                        match inner.next() {
                            Some(v) => v,
                            None => return Err(source.with(Error::MalformedIf)),
                        },
                    )?;
                    let if_false = inner.next().map(|v| to_expression(ctx, v)).transpose()?;
                    if inner.next().is_some() {
                        return Err(source.with(Error::MalformedIf));
                    }
                    if is_if_bind {
                        Ok(source.with(Expression::IfBind(
                            cond.into(),
                            if_true.into(),
                            if_false.map(|v| v.into()),
                        )))
                    } else {
                        Ok(source.with(Expression::If(
                            cond.into(),
                            if_true.into(),
                            if_false.map(|v| v.into()),
                        )))
                    }
                } else {
                    // If first value is a string, infer colon.
                    let mut f = f;
                    if let Tree::String(_) = &*f {
                        f = f.source().with(Tree::Colon(f.into()));
                    }
                    let f = to_expression(ctx.binding(false).allow_merge(false), f)?;
                    let rest = inner
                        .map(|v| to_expression(ctx.allow_merge(true), v))
                        .collect::<Result<Vec<_>, _>>()?;
                    if ctx.binding {
                        Ok(source.with(Expression::BindCommand(f.into(), rest)))
                    } else {
                        Ok(source.with(Expression::Command(f.into(), rest)))
                    }
                }
            }
        }
        Tree::Curly(inner) => Parser::with_context(
            inner.into_iter().map(Ok),
            ctx.allow_merge(true).bind_equal(false),
        )
        .collect::<Result<Vec<_>, _>>()
        .map(|v| source.with(Expression::Block(v))),
        Tree::Bracket(inner) => {
            Parser::with_context(inner.into_iter().map(Ok), ctx.allow_merge(true))
                .collect::<Result<Vec<_>, _>>()
                .map(|v| source.with(Expression::Array(v)))
        }
        Tree::DocComment(_) => Err(source.with(Error::BadDocComment)),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::parse_tree::Parser as TreeParser;
    use crate::ast::tokenize::Tokens;
    use crate::ast::tokenize_tree::TreeTokens;

    use Expression::*;

    fn src<T, R>(t: T) -> R
    where
        R: From<Source<T>>,
    {
        Source::builtin(t).into()
    }

    fn s<R>(s: &str) -> R
    where
        R: From<Source<Expression>>,
    {
        src(String(s.into()))
    }

    fn merge<R>(e: Source<Expression>) -> R
    where
        R: From<Source<Expression>>,
    {
        src(Merge(e.into()))
    }

    #[test]
    fn empty() {
        assert_single("()", Empty);
    }

    #[test]
    fn string() {
        assert_single("str", String("str".into()));
    }

    #[test]
    fn command() {
        assert_single("echo howdy", Command(src(Get(s("echo"))), vec![s("howdy")]));
    }

    #[test]
    fn nested_commands() {
        assert_single(
            "a ((b c) d) e",
            Command(
                src(Get(s("a"))),
                vec![
                    src(Command(
                        src(Command(src(Get(s("b"))), vec![s("c")])),
                        vec![s("d")],
                    )),
                    s("e"),
                ],
            ),
        );
    }

    #[test]
    fn command_no_args() {
        assert_single("(f)", Command(src(Get(s("f"))), vec![]));
        assert_single(
            "((mod f))",
            Command(src(Command(src(Get(s("mod"))), vec![s("f")])), vec![]),
        );
    }

    #[test]
    fn force_command() {
        assert_single(
            "!echo howdy",
            Force(src(Command(src(Get(s("echo"))), vec![s("howdy")]))),
        );
        assert_single(
            "! echo howdy",
            Force(src(Command(src(Get(s("echo"))), vec![s("howdy")]))),
        );
    }

    #[test]
    fn force_arg() {
        assert_single(
            "echo !howdy",
            Command(src(Get(s("echo"))), vec![src(Force(s("howdy")))]),
        );
    }

    #[test]
    fn bind() {
        assert_single(
            ":a=(echo)",
            Bind(src(Set(s("a"))), src(Command(src(Get(s("echo"))), vec![]))),
        );
        assert_single("::a=a", Bind(src(Set(src(Get(s("a"))))), s("a")));
    }

    #[test]
    fn bind_string_set() {
        assert_single("a = b", Bind(src(Set(s("a"))), s("b")));
        assert_single(":a = b", Bind(src(Set(s("a"))), s("b")));
        assert_single("!a = b", Bind(s("a"), s("b")));
    }

    #[test]
    fn bind_command() {
        assert_single(
            "cmd a = cmd b",
            Bind(
                src(BindCommand(src(Get(s("cmd"))), vec![s("a")])),
                src(Command(src(Get(s("cmd"))), vec![s("b")])),
            ),
        );
    }

    #[test]
    fn bind_equal() {
        assert_single(
            ":a = :b = c",
            Bind(src(BindEqual(src(Set(s("a"))), src(Set(s("b"))))), s("c")),
        );
    }

    #[test]
    fn bind_array() {
        assert_single(
            "[:a,^:b,:c=d,!(:e=:f)] = v",
            Bind(
                src(Array(vec![
                    src(Set(s("a"))),
                    src(Merge(src(Set(s("b"))))),
                    src(BindEqual(src(Set(s("c"))), s("d"))),
                    src(Bind(src(Set(s("e"))), src(Set(s("f"))))),
                ])),
                s("v"),
            ),
        )
    }

    #[test]
    fn bind_map() {
        assert_single(
            "{a,:b=:c,!(:d=:e),:f=(:g=:h)} = v",
            Bind(
                src(Block(vec![
                    src(Bind(src(Set(s("a"))), src(Set(s("a"))))),
                    src(Bind(src(Set(s("b"))), src(Set(s("c"))))),
                    src(Bind(src(Set(s("d"))), src(Set(s("e"))))),
                    src(Bind(
                        src(Set(s("f"))),
                        src(BindEqual(src(Set(s("g"))), src(Set(s("h"))))),
                    )),
                ])),
                s("v"),
            ),
        );
    }

    #[test]
    fn bind_command_eq() {
        assert_single(
            "cmd (a=b) !(:a=:b) = v",
            Bind(
                src(BindCommand(
                    src(Get(s("cmd"))),
                    vec![
                        src(BindEqual(s("a"), s("b"))),
                        src(Bind(src(Set(s("a"))), src(Set(s("b"))))),
                    ],
                )),
                s("v"),
            ),
        );
    }

    #[test]
    fn bind_expr() {
        assert_single("!:a = b", Bind(src(Get(s("a"))), s("b")));
    }

    #[test]
    fn bind_any() {
        assert_single("_ = a", Bind(src(BindAny), s("a")));
    }

    #[test]
    fn get() {
        assert_single(":a", Get(s("a")));
    }

    #[test]
    fn array() {
        assert_single(
            "[ a , b ;\nc\nd ]",
            Array(vec![s("a"), s("b"), s("c"), s("d")]),
        );
    }

    #[test]
    fn block() {
        assert_single(
            "{ a\n () \n:c = (echo)\n}",
            Block(vec![
                src(Bind(src(Set(s("a"))), src(Get(s("a"))))),
                src(Empty),
                src(Bind(
                    src(Set(s("c"))),
                    src(Command(src(Get(s("echo"))), vec![])),
                )),
            ]),
        );
    }

    #[test]
    fn function() {
        assert_single(
            "fn ^_ -> howdy",
            Function(
                src(BindCommand(src(Get(s("fn"))), vec![merge(src(BindAny))])),
                s("howdy"),
            ),
        );
        assert_single(
            "a -> b -> c",
            Function(s("a"), src(Function(s("b"), s("c")))),
        );
        assert_single(
            "pat :a -> :b -> (!:a = :b)",
            Function(
                src(BindCommand(src(Get(s("pat"))), vec![src(Set(s("a")))])),
                src(Function(
                    src(Set(s("b"))),
                    src(Bind(src(Get(s("a"))), src(Get(s("b"))))),
                )),
            ),
        );
    }

    #[test]
    fn if_expr() {
        assert_single("if a b c", If(s("a"), s("b"), Some(s("c"))));
        assert_single("if a b", If(s("a"), s("b"), None));
    }

    #[test]
    fn if_bind() {
        assert_single(
            "if (!a=b) c d",
            IfBind(src(Bind(s("a"), s("b"))), s("c"), Some(s("d"))),
        );
        assert_single(
            "if (!a=b) c",
            IfBind(src(Bind(s("a"), s("b"))), s("c"), None),
        );
    }

    #[test]
    fn merge_array() {
        assert_single("[^[b]]", Array(vec![merge(src(Array(vec![s("b")])))]));
    }

    #[test]
    fn merge_block() {
        assert_single(
            "{^{:a=b}}",
            Block(vec![merge(src(Block(vec![src(Bind(
                src(Set(s("a"))),
                s("b"),
            ))])))]),
        );
    }

    #[test]
    fn merge_command() {
        assert_single(
            "a ^[b] ^{:c=d}",
            Command(
                src(Get(s("a"))),
                vec![
                    merge(src(Array(vec![s("b")]))),
                    merge(src(Block(vec![src(Bind(src(Set(s("c"))), s("d")))]))),
                ],
            ),
        );
    }

    #[test]
    fn doc_comment() {
        assert_single(
            "## my doc\n ##\n##   docs\n##more doc\n##\nval",
            DocComment("my doc\n\n  docs\nmore doc".into(), s("val")),
        );
    }

    #[test]
    fn doc_comment_bind() {
        assert_single(
            "## my doc\n:a = b",
            Bind(src(Set(s("a"))), src(DocComment("my doc".into(), s("b")))),
        );
    }

    fn assert_single(s: &str, expected: Expression) {
        let expr = single(s);
        dbg!(&expr);
        assert!(expr == expected);
    }

    fn single(s: &str) -> Expression {
        Parser::from(TreeParser::from(TreeTokens::from(Tokens::from(
            Source::new(super::super::StringSource::new("<string>", s.to_owned()))
                .open()
                .unwrap(),
        ))))
        .next()
        .unwrap()
        .unwrap()
        .unwrap()
    }

    fn assert_fail(s: &str) {
        Parser::from(TreeParser::from(TreeTokens::from(Tokens::from(
            Source::new(super::super::StringSource::new("<string>", s.to_owned()))
                .open()
                .unwrap(),
        ))))
        .next()
        .unwrap()
        .unwrap_err();
    }
}
