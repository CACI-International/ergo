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

impl<I, E> Iterator for Parser<I>
where
    I: Iterator<Item = Result<Source<Tree>, Source<E>>>,
{
    type Item = Result<BlockItem, Source<Error<E>>>;

    fn next(&mut self) -> Option<Self::Item> {
        to_doc_block_item(self.ctx, &mut self.trees)
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
    /// A doc comment was in an invalid position.
    BadDocComment,
}

impl<E: fmt::Display> fmt::Display for Error<E> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::TreeParse(t) => write!(f, "{}", t),
            Error::BadCaret => write!(f, "cannot merge values here"),
            Error::BadEqual => write!(f, "cannot bind values here"),
            Error::BadDocComment => write!(
                f,
                "doc comments can only be within blocks or brackets, preceding an expression or binding"
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

fn documented<I: Iterator<Item = Result<Source<Tree>, Source<E>>>, E, Item, R>(
    ctx: ParseContext,
    iter: &mut I,
    item: Item,
) -> Option<Result<R, Source<Error<E>>>>
where
    Item: FnOnce(
        ParseContext,
        Option<Source<Vec<DocCommentPart>>>,
        Source<Tree>,
    ) -> Result<R, Source<Error<E>>>,
{
    struct Comment {
        pub parts: Vec<DocCommentPart>,
        pub source: Source<()>,
    }

    let mut doc_comment: Option<Comment> = None;

    let merge_part = |doc_comment: &mut Option<Comment>, part, src| match doc_comment {
        None => {
            *doc_comment = Some(Comment {
                parts: vec![part],
                source: src,
            });
        }
        Some(Comment { parts, source }) => {
            parts.push(part);
            *source = (source.clone(), src).into_source().with(());
        }
    };

    loop {
        match iter.next() {
            None => match doc_comment {
                None => break None,
                Some(Comment { source, .. }) => break Some(Err(source.with(Error::BadDocComment))),
            },
            Some(Err(e)) => break Some(Err(e.map(Error::TreeParse))),
            Some(Ok(t)) => {
                let (source, t) = t.take();
                match t {
                    Tree::DocString(s) => {
                        merge_part(&mut doc_comment, DocCommentPart::String(s), source);
                    }
                    Tree::DocCurly(inner) => {
                        let mut inner = inner.into_iter().map(Ok);
                        let exprs = std::iter::from_fn(move || to_doc_block_item(ctx, &mut inner))
                            .collect::<Result<Vec<_>, _>>();
                        let exprs = match exprs {
                            Err(e) => break Some(Err(e)),
                            Ok(v) => v,
                        };
                        merge_part(
                            &mut doc_comment,
                            DocCommentPart::ExpressionBlock(exprs),
                            source,
                        );
                    }
                    o => {
                        let doc_comment =
                            doc_comment.map(|c| c.source.with(clean_doc_comment(c.parts)));
                        break Some(item(ctx, doc_comment, source.with(o)));
                    }
                }
            }
        }
    }
}

fn clean_doc_comment(parts: Vec<DocCommentPart>) -> Vec<DocCommentPart> {
    // Reduce doc comment parts, trimming whitespace and inserting
    // newlines.
    let mut parts = parts.into_iter();
    let mut result = Vec::new();

    // Use whitespace of first non-empty line of doc comment to
    // determine trimmable whitespace from subsequent lines.
    let mut leading_ws_offset = 0;
    while let Some(part) = parts.next() {
        match part {
            DocCommentPart::String(s) => match s.find(|c: char| !c.is_whitespace()) {
                None => continue,
                Some(offset) => {
                    leading_ws_offset = offset;
                    result.push(DocCommentPart::String(
                        unsafe { s.get_unchecked(offset..) }.into(),
                    ));
                    break;
                }
            },
            DocCommentPart::ExpressionBlock(_) => {
                result.push(part);
                break;
            }
        }
    }

    // Get remaining lines and trim leading whitespace if necessary.
    while let Some(part) = parts.next() {
        match part {
            DocCommentPart::String(ref s) => {
                if let Some(DocCommentPart::String(l)) = result.last_mut() {
                    let offset = std::cmp::min(
                        leading_ws_offset,
                        s.find(|c: char| !c.is_whitespace()).unwrap_or(s.len()),
                    );
                    l.push('\n');
                    l.push_str(unsafe { s.get_unchecked(offset..) });
                } else {
                    result.push(part);
                }
            }
            DocCommentPart::ExpressionBlock(_) => {
                result.push(part);
            }
        }
    }

    // We already have done the equivalent of trim_start, so just need
    // to trim_end.
    if let Some(DocCommentPart::String(l)) = result.last_mut() {
        *l = l.trim_end().into();
    }
    result
}

fn to_block_item<E>(ctx: ParseContext, t: Source<Tree>) -> Result<BlockItem, Source<Error<E>>> {
    let (source, t) = t.take();

    match t {
        // string_implies should only affect direct descendants, set to None for caret and equal
        Tree::Caret(t) => Ok(BlockItem::Merge(to_expression(
            ctx.string_implies(StringImplies::None),
            *t,
        )?)),
        Tree::Equal(a, b) => Ok(BlockItem::Bind(
            to_expression(ctx.pattern(true).string_implies(StringImplies::Set), *a)?,
            to_expression(ctx.string_implies(StringImplies::None), *b)?,
        )),
        t => to_expression(ctx, source.with(t)).map(BlockItem::Expr),
    }
}

fn to_doc_block_item<I: Iterator<Item = Result<Source<Tree>, Source<E>>>, E>(
    ctx: ParseContext,
    iter: &mut I,
) -> Option<Result<BlockItem, Source<Error<E>>>> {
    documented(ctx, iter, |ctx, doc_comment, tree| {
        let mut i = to_block_item(ctx, tree)?;
        match doc_comment {
            None => Ok(i),
            Some(c) => match &mut i {
                BlockItem::Expr(e) | BlockItem::Bind(_, e) => {
                    let old_e = std::mem::replace(e, Source::builtin(Expression::unit()));
                    *e = c.map(|parts| Expression::doc_comment(parts, old_e));
                    Ok(i)
                }
                BlockItem::Merge(_) => Err(c.with(Error::BadDocComment)),
            },
        }
    })
}

fn to_array_item<E>(ctx: ParseContext, t: Source<Tree>) -> Result<ArrayItem, Source<Error<E>>> {
    let (source, t) = t.take();

    match t {
        // string_implies should only affect direct descendants, set to None for caret
        Tree::Caret(t) => Ok(ArrayItem::Merge(to_expression(
            ctx.string_implies(StringImplies::None),
            *t,
        )?)),
        t => to_expression(ctx, source.with(t)).map(ArrayItem::Expr),
    }
}

fn to_doc_array_item<I: Iterator<Item = Result<Source<Tree>, Source<E>>>, E>(
    ctx: ParseContext,
    iter: &mut I,
) -> Option<Result<ArrayItem, Source<Error<E>>>> {
    documented(ctx, iter, |ctx, doc_comment, tree| {
        let mut i = to_array_item(ctx, tree)?;
        match doc_comment {
            None => Ok(i),
            Some(c) => match &mut i {
                ArrayItem::Expr(e) => {
                    let old_e = std::mem::replace(e, Source::builtin(Expression::unit()));
                    *e = c.map(|parts| Expression::doc_comment(parts, old_e));
                    Ok(i)
                }
                ArrayItem::Merge(_) => Err(c.with(Error::BadDocComment)),
            },
        }
    })
}

fn to_expression<E>(
    mut ctx: ParseContext,
    t: Source<Tree>,
) -> Result<Source<Expression>, Source<Error<E>>> {
    let (source, t) = t.take();
    // string_implies should only affect direct descendants
    let string_implies = ctx.string_implies;
    ctx = ctx.string_implies(StringImplies::None);
    match t {
        Tree::String(s, quoted) => {
            if ctx.pattern && s == "_" && !quoted {
                Ok(source.with(Expression::bind_any()))
            } else {
                let s = source.clone().with(Expression::string(s));
                Ok(match string_implies {
                    StringImplies::Set => source.with(Expression::set(s)),
                    StringImplies::Get => source.with(Expression::get(s)),
                    StringImplies::None => s,
                })
            }
        }
        Tree::Bang(t) => {
            if ctx.pattern {
                // Bang in a pattern interprets as a non-pattern expression
                to_expression(ctx.pattern(false), *t)
            } else {
                Ok(source.with(Expression::force(to_expression(ctx, *t)?)))
            }
        }
        Tree::Caret(_) => {
            // Caret not allowed in expressions
            Err(source.with(Error::BadCaret))
        }
        Tree::ColonPrefix(t) => {
            if ctx.pattern {
                Ok(source.with(Expression::set(to_expression(ctx.pattern(false), *t)?)))
            } else {
                Ok(source.with(Expression::get(to_expression(ctx, *t)?)))
            }
        }
        Tree::Colon(a, b) => Ok(source.with(Expression::index(
            to_expression(ctx.string_implies(StringImplies::Get), *a)?,
            to_expression(ctx, *b)?,
        ))),
        Tree::Equal(_, _) => Err(source.with(Error::BadEqual)),
        Tree::Arrow(a, b) => Ok(source.with(Expression::function(
            to_expression(ctx.pattern(true), *a)?,
            to_expression(ctx.pattern(false), *b)?,
        ))),
        Tree::ColonSuffix(f) => {
            let f = to_expression(ctx.pattern(false).string_implies(StringImplies::Get), *f)?;
            if ctx.pattern {
                Ok(source.with(Expression::pat_command(f, vec![])))
            } else {
                Ok(source.with(Expression::command(f, vec![])))
            }
        }
        Tree::Parens(inner) => {
            let mut inner = inner.into_iter().peekable();
            match inner.next() {
                None => Ok(source.with(Expression::unit())),
                Some(f) => {
                    if inner.peek().is_none() {
                        to_expression(ctx, f)
                    } else {
                        let f = to_expression(
                            ctx.pattern(false).string_implies(StringImplies::Get),
                            f,
                        )?;
                        let rest = inner
                            .map(|v| to_block_item(ctx, v))
                            .collect::<Result<Vec<_>, _>>()?;
                        if ctx.pattern {
                            Ok(source.with(Expression::pat_command(f, rest)))
                        } else {
                            Ok(source.with(Expression::command(f, rest)))
                        }
                    }
                }
            }
        }
        Tree::Curly(inner) => {
            let mut inner = inner.into_iter().map(Ok);
            let exprs = std::iter::from_fn(move || to_doc_block_item(ctx, &mut inner))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(source.with(Expression::block(exprs)))
        }
        Tree::Bracket(inner) => {
            let mut inner = inner.into_iter().map(Ok);
            let exprs = std::iter::from_fn(move || to_doc_array_item(ctx, &mut inner))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(source.with(Expression::array(exprs)))
        }
        Tree::DocString(_) | Tree::DocCurly(_) => Err(source.with(Error::BadDocComment)),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::parse_tree::Parser as TreeParser;
    use crate::ast::tokenize::Tokens;
    use crate::ast::tokenize_tree::TreeTokens;
    use ergo_runtime::source::StringSource;

    type E = Expression;
    type AI = ArrayItem;
    type BI = BlockItem;

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
        src(E::string(s.into()))
    }

    macro_rules! array_items {
        ( @item merge($e:expr) ) => {
            AI::Merge($e)
        };
        ( @item $e:expr ) => {
            AI::Expr($e)
        };
        ( @go [$(($($out:tt)+))*]) => {
            vec![$(array_items!(@item $($out)+)),*]
        };
        ( @go [$($out:tt)*] []) => {
            array_items!(@go [$($out)*]);
        };
        ( @go [$($out:tt)*] [$($pending:tt)+]) => {
            array_items!(@go [$($out)* ($($pending)+)]);
        };
        ( @go [$($out:tt)*] [$($pending:tt)+] , $($rest:tt)+ ) => {
            array_items!(@go [$($out)* ($($pending)+)] [] $($rest)+);
        };
        ( @go [$($out:tt)*] [$($pending:tt)*] $in:tt $($rest:tt)* ) => {
            array_items!(@go [$($out)*] [$($pending)* $in] $($rest)*);
        };
        ( $($in:tt)* ) => {
            array_items!(@go [] [] $($in)*);
        }
    }

    macro_rules! block_items {
        ( @item merge($e:expr) ) => {
            BI::Merge($e)
        };
        ( @item bind($b:expr, $e:expr) ) => {
            BI::Bind($b, $e)
        };
        ( @item $e:expr ) => {
            BI::Expr($e)
        };
        ( @go [$(($($out:tt)+))*]) => {
            vec![$(block_items!(@item $($out)+)),*]
        };
        ( @go [$($out:tt)*] []) => {
            block_items!(@go [$($out)*]);
        };
        ( @go [$($out:tt)*] [$($pending:tt)+]) => {
            block_items!(@go [$($out)* ($($pending)+)]);
        };
        ( @go [$($out:tt)*] [$($pending:tt)+] , $($rest:tt)+ ) => {
            block_items!(@go [$($out)* ($($pending)+)] [] $($rest)+);
        };
        ( @go [$($out:tt)*] [$($pending:tt)*] $in:tt $($rest:tt)* ) => {
            block_items!(@go [$($out)*] [$($pending)* $in] $($rest)*);
        };
        ( $($in:tt)* ) => {
            block_items!(@go [] [] $($in)*);
        }
    }

    macro_rules! command_items {
        ( $($in:tt)* ) => {
            block_items!($($in)*);
        }
    }

    #[test]
    fn unit() {
        assert_single("()", E::unit());
    }

    #[test]
    fn string() {
        assert_single("str", E::string("str".into()));
    }

    #[test]
    fn command() {
        assert_single(
            "echo howdy",
            E::command(src(E::get(s("echo"))), command_items![s("howdy")]),
        );
    }

    #[test]
    fn nested_commands() {
        assert_single(
            "a ((b c) d) e",
            E::command(
                src(E::get(s("a"))),
                command_items![
                    src(E::command(
                        src(E::command(src(E::get(s("b"))), command_items![s("c")])),
                        command_items![s("d")]
                    )),
                    s("e")
                ],
            ),
        );
    }

    #[test]
    fn command_no_args() {
        assert_single("f:", E::command(src(E::get(s("f"))), command_items![]));
        assert_single(
            "(mod f):",
            E::command(
                src(E::command(src(E::get(s("mod"))), command_items![s("f")])),
                command_items![],
            ),
        );
    }

    #[test]
    fn force_command() {
        assert_single(
            "!echo howdy",
            E::force(src(E::command(
                src(E::get(s("echo"))),
                command_items![s("howdy")],
            ))),
        );
        assert_single(
            "! echo howdy",
            E::force(src(E::command(
                src(E::get(s("echo"))),
                command_items![s("howdy")],
            ))),
        );
    }

    #[test]
    fn force_arg() {
        assert_single(
            "echo !howdy",
            E::command(
                src(E::get(s("echo"))),
                command_items![src(E::force(s("howdy")))],
            ),
        );
    }

    #[test]
    fn command_eq() {
        assert_single(
            "hello (name=world)",
            E::command(
                src(E::get(s("hello"))),
                command_items![bind(src(E::set(s("name"))), s("world"))],
            ),
        );
    }

    #[test]
    fn bind() {
        assert_block_item(
            ":a= echo:",
            BI::Bind(
                src(E::set(s("a"))),
                src(E::command(src(E::get(s("echo"))), command_items![])),
            ),
        );
        assert_block_item("::a=a", BI::Bind(src(E::set(src(E::get(s("a"))))), s("a")));
    }

    #[test]
    fn bind_string_set() {
        assert_block_item("a = b", BI::Bind(src(E::set(s("a"))), s("b")));
        assert_block_item(":a = b", BI::Bind(src(E::set(s("a"))), s("b")));
        assert_block_item("!a = b", BI::Bind(s("a"), s("b")));
    }

    #[test]
    fn pat_command() {
        assert_block_item(
            "cmd a = cmd b",
            BI::Bind(
                src(E::pat_command(
                    src(E::get(s("cmd"))),
                    command_items![s("a")],
                )),
                src(E::command(src(E::get(s("cmd"))), command_items![s("b")])),
            ),
        );
    }

    #[test]
    fn extra_equal() {
        assert_fail(":a = :b = c");
    }

    #[test]
    fn bind_array() {
        assert_block_item(
            "[:a,^:b] = v",
            BI::Bind(
                src(E::array(array_items![
                    src(E::set(s("a"))),
                    merge(src(E::set(s("b"))))
                ])),
                s("v"),
            ),
        )
    }

    #[test]
    fn bind_map() {
        assert_block_item(
            "{a,:b=:c,:d=!:e,^:rest} = v",
            BI::Bind(
                src(E::block(block_items![
                    bind(src(E::set(s("a"))), src(E::set(s("a")))),
                    bind(src(E::set(s("b"))), src(E::set(s("c")))),
                    bind(src(E::set(s("d"))), src(E::get(s("e")))),
                    merge(src(E::set(s("rest"))))
                ])),
                s("v"),
            ),
        );
    }

    #[test]
    fn pat_command_eq() {
        assert_block_item(
            "cmd (a=b) = v",
            BI::Bind(
                src(E::pat_command(
                    src(E::get(s("cmd"))),
                    command_items![bind(src(E::set(s("a"))), s("b"))],
                )),
                s("v"),
            ),
        );
    }

    #[test]
    fn bind_expr() {
        assert_block_item("!:a = b", BI::Bind(src(E::get(s("a"))), s("b")));
    }

    #[test]
    fn bind_any() {
        assert_block_item("_ = _", BI::Bind(src(E::bind_any()), s("_")));
    }

    #[test]
    fn get() {
        assert_single(":a", E::get(s("a")));
    }

    #[test]
    fn array() {
        assert_single(
            "[ a , b ;\nc\nd ]",
            E::array(array_items![s("a"), s("b"), s("c"), s("d")]),
        );
    }

    #[test]
    fn map() {
        assert_single(
            "{ a, :c = hello }",
            E::block(block_items![
                bind(src(E::set(s("a"))), src(E::get(s("a")))),
                bind(src(E::set(s("c"))), s("hello"))
            ]),
        );
        assert_single("{}", E::block(block_items![]));
        assert_single(
            "{a}",
            E::block(block_items![bind(src(E::set(s("a"))), src(E::get(s("a"))))]),
        );
    }

    #[test]
    fn block() {
        assert_single(
            "{ a\n () \n:c = echo:\n()}",
            E::block(block_items![
                bind(src(E::set(s("a"))), src(E::get(s("a")))),
                src(E::unit()),
                bind(
                    src(E::set(s("c"))),
                    src(E::command(src(E::get(s("echo"))), command_items![]))
                ),
                src(E::unit())
            ]),
        );
    }

    #[test]
    fn function() {
        assert_single(
            "fn ^_ -> howdy",
            E::function(
                src(E::pat_command(
                    src(E::get(s("fn"))),
                    command_items![merge(src(E::bind_any()))],
                )),
                s("howdy"),
            ),
        );
        assert_single(
            "a -> b -> c",
            E::function(s("a"), src(E::function(s("b"), s("c")))),
        );
        assert_single(
            "pat :a -> :b -> {!:a = :b}",
            E::function(
                src(E::pat_command(
                    src(E::get(s("pat"))),
                    command_items![src(E::set(s("a")))],
                )),
                src(E::function(
                    src(E::set(s("b"))),
                    src(E::block(block_items![bind(
                        src(E::get(s("a"))),
                        src(E::get(s("b")))
                    )])),
                )),
            ),
        );
    }

    #[test]
    fn merge_array() {
        assert_single(
            "[^[b]]",
            E::array(array_items![merge(src(E::array(array_items![s("b")])))]),
        );
    }

    #[test]
    fn bind_in_array() {
        assert_fail("[a=b]");
    }

    #[test]
    fn merge_block() {
        assert_single(
            "{^{:a=b}}",
            E::block(block_items![merge(src(E::block(block_items![bind(
                src(E::set(s("a"))),
                s("b")
            )])))]),
        );
    }

    #[test]
    fn merge_command() {
        assert_single(
            "a ^[b] ^{:c=d}",
            E::command(
                src(E::get(s("a"))),
                command_items![
                    merge(src(E::array(array_items![s("b")]))),
                    merge(src(E::block(block_items![bind(
                        src(E::set(s("c"))),
                        s("d")
                    )])))
                ],
            ),
        );
    }

    #[test]
    fn doc_comment() {
        assert_single(
            "## my doc\n ##\n##   docs\n##more doc\n##\nval",
            E::doc_comment(
                vec![DocCommentPart::String("my doc\n\n  docs\nmore doc".into())],
                s("val"),
            ),
        );
    }

    #[test]
    fn doc_comment_bind() {
        assert_block_item(
            "## my doc\n:a = b",
            BI::Bind(
                src(E::set(s("a"))),
                src(E::doc_comment(
                    vec![DocCommentPart::String("my doc".into())],
                    s("b"),
                )),
            ),
        );
    }

    #[test]
    fn doc_comment_expressions() {
        assert_single(
            "## my doc {{ comment }}\n## {{\n## more expr\n## abc def ghi }} stuff\n##   more docs\nval",
            E::doc_comment(
                vec![
                    DocCommentPart::String("my doc ".into()),
                    DocCommentPart::ExpressionBlock(block_items![s("comment")]),
                    DocCommentPart::String("\n".into()),
                    DocCommentPart::ExpressionBlock(block_items![
                        src(E::command(src(E::get(s("more"))), command_items![s("expr")])),
                        src(E::command(src(E::get(s("abc"))), command_items![s("def"), s("ghi")]))
                    ]),
                    DocCommentPart::String(" stuff\n  more docs".into()),
                ],
                s("val"),
            ),
        );
    }

    fn assert_single(s: &str, expected: Expression) {
        let expr = single(s);
        dbg!(&expr);
        assert!(expr.id_eq(&expected));
        assert!(expr == expected);
    }

    fn assert_block_item(s: &str, expected: BI) {
        let item = block_item(s);
        dbg!(&item);
        assert!(item == expected);
    }

    fn single(s: &str) -> Expression {
        match block_item(s) {
            BlockItem::Expr(e) => e.unwrap(),
            _ => panic!("unexpected block item"),
        }
    }

    fn block_item(s: &str) -> BI {
        Parser::from(TreeParser::from(TreeTokens::from(Tokens::from(
            Source::new(StringSource::new("<string>", s.to_owned()))
                .open()
                .unwrap(),
        ))))
        .next()
        .unwrap()
        .unwrap()
    }

    fn assert_fail(s: &str) {
        Parser::from(TreeParser::from(TreeTokens::from(Tokens::from(
            Source::new(StringSource::new("<string>", s.to_owned()))
                .open()
                .unwrap(),
        ))))
        .next()
        .unwrap()
        .unwrap_err();
    }
}
