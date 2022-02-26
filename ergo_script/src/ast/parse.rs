//! AST parsing.
//!
//! The parser takes an iterator of parsed `parse_tree::Tree`s and produces an iterator of
//! top-level `BlockItem` expressions. In doing so some syntax is desugared, such as bare strings
//! implying get, set, or bind operations (depending on context). Commands and pattern commands are
//! also disambiguated depending on context.

use super::parse_tree::{StringTree, Tree};
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
    T: IntoIterator<Item = Result<Source<Tree>, Vec<Source<E>>>>,
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
    I: Iterator<Item = Result<Source<Tree>, Vec<Source<E>>>>,
{
    type Item = Result<BlockItem, Vec<Source<Error<E>>>>;

    fn next(&mut self) -> Option<Self::Item> {
        to_block_item(self.ctx, &mut self.trees, false)
    }
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
                    Tree::Hash(_) | Tree::HashBlock(_) => continue,
                    _ => break Some(Ok(t)),
                },
            }
        }
    }
}

struct NoComments<T>(T);

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
                    Tree::Hash(_) | Tree::HashBlock(_) => continue,
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
    /// A dollar operator was in an invalid position.
    BadDollar,
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
            Error::BadDollar => write!(f, "a dollar must be followed by a string"),
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

impl<E: super::ToDiagnostic + fmt::Display> super::ToDiagnostic for Error<E> {
    fn additional_info(&self, diagnostic: &mut ergo_runtime::error::Diagnostic) {
        match self {
            Error::TreeParse(e) => e.additional_info(diagnostic),
            _ => (),
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
    ctx: ParseContext,
    iter: &mut I,
    parse: Parse,
    sub_expr: SubExpr,
) -> Option<Result<R, Vec<Source<Error<E>>>>>
where
    Parse: FnOnce(ParseContext, Source<Tree>) -> Result<R, Vec<Source<Error<E>>>>,
    SubExpr: for<'a> FnOnce(&'a mut R, &mut I) -> Option<&'a mut Expr>,
{
    enum Attr {
        DocComment(Vec<StringItem>),
        Attribute(Source<Expression>),
    }
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
                    Tree::DoubleHashBlock(parts) => match string_parts(ctx, parts) {
                        Ok(v) => attributes.push(source.with(Attr::DocComment(v))),
                        Err(e) => break Some(Err(e)),
                    },
                    Tree::DoubleHash(inner) => {
                        match to_expression(
                            ctx.pattern(false).string_implies(StringImplies::Get),
                            *inner,
                        ) {
                            Ok(v) => attributes.push(source.with(Attr::Attribute(v))),
                            Err(e) => break Some(Err(e)),
                        }
                    }
                    o => {
                        let mut v = match parse(ctx, source.with(o)) {
                            Err(e) => break Some(Err(e)),
                            Ok(v) => v,
                        };
                        if !attributes.is_empty() {
                            if let Some(e) = sub_expr(&mut v, iter.0) {
                                while let Some(attr) = attributes.pop() {
                                    let old_e =
                                        std::mem::replace(e, Source::missing(Expression::unit()));
                                    *e = attr.map(|a| match a {
                                        Attr::Attribute(a) => Expression::attribute(a, old_e),
                                        Attr::DocComment(parts) => {
                                            Expression::doc_comment(parts, old_e)
                                        }
                                    });
                                }
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
    ctx: ParseContext,
    trees: Vec<Source<StringTree>>,
) -> Result<Vec<StringItem>, Vec<Source<Error<E>>>> {
    // TODO catch more than one error (don't use collect())?
    trees
        .into_iter()
        .map(|p| {
            Ok(match p.unwrap() {
                StringTree::String(s) => StringItem::String(s),
                StringTree::Expression(tree) => StringItem::Expression(to_expression(
                    ctx.pattern(false).string_implies(StringImplies::Get),
                    tree,
                )?),
            })
        })
        .collect()
}

fn to_basic_block_item<E>(
    ctx: ParseContext,
    t: Source<Tree>,
    string_implies_bind: bool,
) -> Result<BlockItem, Vec<Source<Error<E>>>> {
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
        // elaborate an unquoted string literal `a` to `:a = $a`
        Tree::String(s) if string_implies_bind => {
            let s = source.clone().with(Expression::string(s));
            Ok(BlockItem::Bind(
                source.clone().with(Expression::set(s.clone())),
                source.with(if ctx.pattern {
                    Expression::set(s)
                } else {
                    Expression::get(s)
                }),
            ))
        }
        t => to_expression(ctx, source.with(t)).map(BlockItem::Expr),
    }
}

fn to_block_item<I: Iterator<Item = Result<Source<Tree>, Vec<Source<E>>>>, E>(
    ctx: ParseContext,
    iter: &mut I,
    string_implies_bind: bool,
) -> Option<Result<BlockItem, Vec<Source<Error<E>>>>> {
    attributed(
        ctx,
        iter,
        |ctx, t| to_basic_block_item(ctx, t, string_implies_bind),
        |block_item, _| match block_item {
            BlockItem::Expr(e) | BlockItem::Bind(_, e) => Some(e),
            BlockItem::Merge(_) => None,
        },
    )
}

fn to_basic_array_item<E>(
    ctx: ParseContext,
    t: Source<Tree>,
) -> Result<ArrayItem, Vec<Source<Error<E>>>> {
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

fn to_array_item<I: Iterator<Item = Result<Source<Tree>, Vec<Source<E>>>>, E>(
    ctx: ParseContext,
    iter: &mut I,
) -> Option<Result<ArrayItem, Vec<Source<Error<E>>>>> {
    attributed(
        ctx,
        iter,
        to_basic_array_item,
        |array_item, _| match array_item {
            ArrayItem::Expr(e) => Some(e),
            ArrayItem::Merge(_) => None,
        },
    )
}

/*
std:import { :a, :b, c++ => {:module} } = :workspace

{ a, b, c } ==> { a = $a, b = $b, c = $c }
{ :a, :b, :c } ==> { a => :a, b => :b, c => :c }

let :a = 123
let :b = (let :a = 123)

fn let :a = ^:args -> a:$v

Path:with :f1 :f2 :f3 {
    std:exec cp $f1 $f2
}

path-with = fn ^:fs :v -> {
    std:Iter:map (:f -> {$f = Path:new ()}) $fs
    $v
}

something (setter => :blah)

Order matters if you want to use `$`... or `$` needs to block

fn {
    debug => default :debug as true
    debug-symbols => default :debug-symbols as $debug
}
*/

fn to_expression<E>(
    mut ctx: ParseContext,
    t: Source<Tree>,
) -> Result<Source<Expression>, Vec<Source<Error<E>>>> {
    let (source, t) = t.take();
    // string_implies should only affect direct descendants
    let string_implies = ctx.string_implies;
    ctx = ctx.string_implies(StringImplies::None);
    match t {
        Tree::String(s) => {
            if ctx.pattern && s == "_" {
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
        Tree::Caret(_) => {
            // Caret not allowed in expressions
            Err(vec![source.with(Error::BadCaret)])
        }
        Tree::Dollar(t) => {
            // Dollar may only be followed by a string.
            let inner = to_expression(ctx, *t)?;
            if inner.expr_type() == ExpressionType::String {
                Ok(source.with(Expression::get(inner)))
            } else {
                Err(vec![source.with(Error::BadDollar)])
            }
        }
        Tree::ColonPrefix(t) => {
            if ctx.pattern {
                Ok(source.with(Expression::set(to_expression(ctx.pattern(false), *t)?)))
            } else {
                Ok(source.with(Expression::get(to_expression(ctx, *t)?)))
            }
        }
        Tree::Equal(_, _) => Err(vec![source.with(Error::BadEqual)]),
        Tree::Arrow(a, b) => Ok(source.with(Expression::function(
            to_expression(ctx.pattern(true), *a)?,
            to_expression(ctx.pattern(false), *b)?,
        ))),
        Tree::Colon(a, b) => Ok(source.with(Expression::index(
            to_expression(ctx.string_implies(StringImplies::Get), *a)?,
            to_expression(ctx, *b)?,
        ))),
        Tree::Parens(inner) => {
            let mut inner = NoComments(inner.into_iter()).peekable();
            match inner.peek() {
                None => Ok(source.with(Expression::unit())),
                Some(f) => {
                    // See if the first item is an attribute, and parse accordingly.
                    match f.value() {
                        Tree::DoubleHash(_) | Tree::DoubleHashBlock(_) => {
                            attributed(
                                ctx,
                                &mut (&mut inner).map(Ok).peekable(),
                                to_expression,
                                |v, inner| {
                                    if inner.peek().is_none() {
                                        Some(v)
                                    } else {
                                        // Attributes cannot be applied if there are multiple values
                                        // following.
                                        None
                                    }
                                },
                            )
                            .unwrap()
                        }
                        _ => {
                            let f = inner.next().unwrap();
                            if inner.peek().is_none() {
                                to_expression(ctx, f)
                            } else {
                                let f = to_expression(
                                    ctx.pattern(false).string_implies(StringImplies::Get),
                                    f,
                                )?;
                                let rest = inner
                                    .map(|v| to_basic_block_item(ctx, v, false))
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
            }
        }
        Tree::Curly(inner) => {
            let mut inner = inner.into_iter().map(Ok);
            let exprs = std::iter::from_fn(move || to_block_item(ctx, &mut inner, true))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(source.with(Expression::block(exprs)))
        }
        Tree::Bracket(inner) => {
            let mut inner = inner.into_iter().map(Ok);
            let exprs = std::iter::from_fn(move || to_array_item(ctx, &mut inner))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(source.with(Expression::array(exprs)))
        }
        Tree::Quote(strings) | Tree::ApostropheBlock(strings) => Ok(source.with(
            if strings.iter().all(|s| match s.value() {
                StringTree::String(_) => true,
                _ => false,
            }) {
                Expression::string(
                    strings
                        .into_iter()
                        .map(|s| match s.unwrap() {
                            StringTree::String(s) => s,
                            _ => panic!("invalid string tree"),
                        })
                        .collect::<Vec<_>>()
                        .join(""),
                )
            } else {
                Expression::compound_string(string_parts(ctx, strings)?)
            },
        )),
        Tree::Hash(_) | Tree::HashBlock(_) => Err(vec![source.with(Error::BadComment)]),
        Tree::DoubleHash(_) | Tree::DoubleHashBlock(_) => {
            Err(vec![source.with(Error::BadAttribute)])
        }
    }
}

#[cfg(test)]
mod test {
    use super::Error;
    use super::*;
    use crate::ast::parse_tree::Parser as TreeParser;
    use crate::ast::tokenize::Tokens;

    type E = Expression;
    type AI = ArrayItem;
    type BI = BlockItem;

    fn src<T, R>(t: T) -> R
    where
        R: From<Source<T>>,
    {
        Source::missing(t).into()
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
            array_items!(@go [$($out)*])
        };
        ( @go [$($out:tt)*] [$($pending:tt)+]) => {
            array_items!(@go [$($out)* ($($pending)+)])
        };
        ( @go [$($out:tt)*] [$($pending:tt)+] , $($rest:tt)+ ) => {
            array_items!(@go [$($out)* ($($pending)+)] [] $($rest)+)
        };
        ( @go [$($out:tt)*] [$($pending:tt)*] $in:tt $($rest:tt)* ) => {
            array_items!(@go [$($out)*] [$($pending)* $in] $($rest)*)
        };
        ( $($in:tt)* ) => {
            array_items!(@go [] [] $($in)*)
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
            block_items!(@go [$($out)*])
        };
        ( @go [$($out:tt)*] [$($pending:tt)+]) => {
            block_items!(@go [$($out)* ($($pending)+)])
        };
        ( @go [$($out:tt)*] [$($pending:tt)+] , $($rest:tt)+ ) => {
            block_items!(@go [$($out)* ($($pending)+)] [] $($rest)+)
        };
        ( @go [$($out:tt)*] [$($pending:tt)*] $in:tt $($rest:tt)* ) => {
            block_items!(@go [$($out)*] [$($pending)* $in] $($rest)*)
        };
        ( $($in:tt)* ) => {
            block_items!(@go [] [] $($in)*)
        }
    }

    macro_rules! command_items {
        ( $($in:tt)* ) => {
            block_items!($($in)*)
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
    fn quoted_string() {
        assert_single("\"\"", E::string("".into()));
        assert_single("\"abc\ndef\"", E::string("abc\ndef".into()));
        assert_single(
            "\"hello $$$world\"",
            E::compound_string(vec![
                StringItem::String("hello ".into()),
                StringItem::String("$".into()),
                StringItem::Expression(src(E::get(s("world")))),
            ]),
        );
    }

    #[test]
    fn block_string() {
        assert_single(
            "' block
        ' string over
        ' multiple lines",
            E::string("block\nstring over\nmultiple lines".into()),
        );
        assert_single(
            "' block
        ' string with $$
        ' and $capture",
            E::compound_string(vec![
                StringItem::String("block\n".into()),
                StringItem::String("string with ".into()),
                StringItem::String("$".into()),
                StringItem::String("\n".into()),
                StringItem::String("and ".into()),
                StringItem::Expression(src(E::get(s("capture")))),
            ]),
        );
    }

    #[test]
    fn comments() {
        assert_single(
            "# this is a comment
        # that continues for a while
        #nope #(not this either) value",
            E::string("value".into()),
        );
    }

    #[test]
    fn attribute() {
        assert_single(
            "##(my attr)
            value",
            E::attribute(
                src(E::command(src(E::get(s("my"))), command_items![s("attr")])),
                s("value"),
            ),
        );
    }

    #[test]
    fn attribute_bind() {
        assert_block_item(
            "##my-attr\na = b",
            BI::Bind(
                src(E::set(s("a"))),
                src(E::attribute(src(E::get(s("my-attr"))), s("b"))),
            ),
        );
    }

    #[test]
    fn attribute_parens() {
        assert_single(
            "##my-attr value",
            E::attribute(src(E::get(s("my-attr"))), s("value")),
        );

        assert_single(
            "a (##my-attr value) b",
            E::command(
                src(E::get(s("a"))),
                command_items![
                    src(E::attribute(src(E::get(s("my-attr"))), s("value"))),
                    s("b")
                ],
            ),
        );
    }

    #[test]
    fn bad_attribute() {
        assert_err("##my-attr a b", Error::BadAttribute);
        assert_err("(## my-doc\na b)", Error::BadAttribute);
    }

    #[test]
    fn doc_comments_and_attributes() {
        assert_single(
            "##top-attr
        ## my doc comment
        ##my-attr
        ##my-attr-2
        ## my doc comment 2
        ## my doc comment 3
        value",
            E::attribute(
                src(E::get(s("top-attr"))),
                src(E::doc_comment(
                    vec![StringItem::String("my doc comment".into())],
                    src(E::attribute(
                        src(E::get(s("my-attr"))),
                        src(E::attribute(
                            src(E::get(s("my-attr-2"))),
                            src(E::doc_comment(
                                vec![
                                    StringItem::String("my doc comment 2\n".into()),
                                    StringItem::String("my doc comment 3".into()),
                                ],
                                s("value"),
                            )),
                        )),
                    )),
                )),
            ),
        );
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
            ":a= echo()",
            BI::Bind(
                src(E::set(s("a"))),
                src(E::command(
                    src(E::get(s("echo"))),
                    command_items![src(E::unit())],
                )),
            ),
        );
        assert_block_item("::a=a", BI::Bind(src(E::set(src(E::get(s("a"))))), s("a")));
    }

    #[test]
    fn bind_string_set() {
        assert_block_item("a = b", BI::Bind(src(E::set(s("a"))), s("b")));
        assert_block_item(":a = b", BI::Bind(src(E::set(s("a"))), s("b")));
        assert_block_item("\"a\" = b", BI::Bind(s("a"), s("b")));
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
        assert_err(":a = :b = c", Error::BadEqual);
    }

    #[test]
    fn trailing_colon() {
        assert_fail("f:");
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
            "{a,:b=:c,:d=$e,^:rest} = v",
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
        assert_block_item("$a = b", BI::Bind(src(E::get(s("a"))), s("b")));
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
            "{ a\n () \n:c = echo()\n()}",
            E::block(block_items![
                bind(src(E::set(s("a"))), src(E::get(s("a")))),
                src(E::unit()),
                bind(
                    src(E::set(s("c"))),
                    src(E::command(
                        src(E::get(s("echo"))),
                        command_items![src(E::unit())]
                    ))
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
            "pat :a -> :b -> {$a = :b}",
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
        assert_err("[a=b]", Error::BadEqual);
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
            "## my doc\n ##\n##   docs\n## more doc\n##\nval",
            E::doc_comment(
                vec![
                    StringItem::String("my doc\n".into()),
                    StringItem::String("\n".into()),
                    StringItem::String("  docs\n".into()),
                    StringItem::String("more doc\n".into()),
                ],
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
                    vec![StringItem::String("my doc".into())],
                    s("b"),
                )),
            ),
        );
    }

    #[test]
    fn doc_comment_expressions() {
        assert_single(
            "## my doc $comment\n## ${\n## more expr\n## abc def ghi} stuff\n##   more docs\nval",
            E::doc_comment(
                vec![
                    StringItem::String("my doc ".into()),
                    StringItem::Expression(src(E::get(s("comment")))),
                    StringItem::String("\n".into()),
                    StringItem::Expression(src(E::block(block_items![
                        src(E::command(
                            src(E::get(s("more"))),
                            command_items![s("expr")]
                        )),
                        src(E::command(
                            src(E::get(s("abc"))),
                            command_items![s("def"), s("ghi")]
                        ))
                    ]))),
                    StringItem::String(" stuff\n".into()),
                    StringItem::String("  more docs".into()),
                ],
                s("val"),
            ),
        );
    }

    #[test]
    fn doc_comment_parens() {
        assert_single(
            "a (## my docs\nvalue) b",
            E::command(
                src(E::get(s("a"))),
                command_items![
                    src(E::doc_comment(
                        vec![StringItem::String("my docs".into())],
                        s("value")
                    )),
                    s("b")
                ],
            ),
        );
    }

    #[test]
    fn bad_caret() {
        assert_err("^^hi", Error::BadCaret);
    }

    #[test]
    fn bad_comment() {
        assert_err("^#hi", Error::BadComment);
    }

    #[test]
    fn shebang() {
        assert_single("#!/usr/bin/env ergo\n()", E::unit());
    }

    fn assert_single(s: &str, expected: Expression) {
        let expr = single(s);
        dbg!(&expr);
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
        Parser::from(TreeParser::from(Tokens::from(Source::missing(s))))
            .next()
            .unwrap()
            .unwrap()
    }

    fn assert_fail(s: &str) {
        Parser::from(TreeParser::from(Tokens::from(Source::missing(s))))
            .next()
            .unwrap()
            .unwrap_err();
    }

    fn assert_err(s: &str, e: Error<super::parse_tree::Error<super::tokenize::Error>>) {
        let err = Parser::from(TreeParser::from(Tokens::from(Source::missing(s))))
            .next()
            .unwrap()
            .unwrap_err()
            .pop()
            .unwrap()
            .unwrap();
        dbg!(&err);
        assert!(e == err);
    }
}
