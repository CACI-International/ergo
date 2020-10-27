//! AST parsing.

use super::tokenize::{Tok, Token};
use super::*;
use ergo_runtime::source::{IntoSource, Source};
use pom::parser::*;
use std::collections::VecDeque;
use std::iter::FromIterator;

/// A parser alias.
pub type Parser<'a, T> = pom::parser::Parser<'a, Tok, T>;

/// Return a parser for a script.
pub fn script<'a>() -> Parser<'a, Script> {
    spacenl()
        * list(
            expression::merge_with(expression::expression(true)),
            delim(),
        )
        .map(|e| e.into_source())
        - spacenl()
        - end()
}

/// Pattern parsing.
mod pattern {
    use super::*;

    /// A pattern.
    pub fn pattern<'a>() -> Parser<'a, Pat> {
        call(|| {
            let any = tag("_").map(|s| s.with(Pattern::Any));
            let binding = word().map(|s| s.map(Pattern::Binding));

            map() | array() | literal() | any | binding
        })
    }

    /// A command pattern.
    pub fn command_pattern<'a>() -> Parser<'a, CmdPat> {
        list(array_item(), req_space()).map(|args| args.into_source())
    }

    /// A literal pattern.
    fn literal<'a>() -> Parser<'a, Pat> {
        (sym(Token::Equal) + expression::arg())
            .map(|m| m.into_source().map(|(_, e)| Pattern::Literal(e)))
    }

    /// An array pattern.
    fn array<'a>() -> Parser<'a, Pat> {
        array_syntax(array_item()).map(|e| e.into_source().map(Pattern::Array))
    }

    /// A map pattern.
    fn map<'a>() -> Parser<'a, Pat> {
        let bind_map_item = (word() + (space() * sym(Token::Equal) * space() * pattern()).opt())
            .map(|(name, binding)| match binding {
                Some(pat) => (name.source(), pat.source())
                    .into_source()
                    .with(MapPattern::Item(name.unwrap(), pat)),
                None => {
                    let src = name.source();
                    name.map(|n| MapPattern::Item(n.clone(), src.with(Pattern::Binding(n))))
                }
            });
        let rest_map_item = (sym(Token::Caret) + pattern())
            .map(|e| e.into_source().map(|(_, p)| MapPattern::Rest(p)));
        let map_item = rest_map_item | bind_map_item;
        block_syntax(map_item).map(|e| e.into_source().map(Pattern::Map))
    }

    /// An array item pattern.
    fn array_item<'a>() -> Parser<'a, Source<ArrayPattern<Expr>>> {
        !function_delim()
            * (sym(Token::Caret).opt() + pattern()).map(|(c, p)| match c {
                Some(c) => (c, p).into_source().map(|(_, p)| ArrayPattern::Rest(p)),
                None => p.source().with(ArrayPattern::Item(p)),
            })
    }
}

/// Expression parsing.
mod expression {
    use super::*;

    /// A set expression.
    fn set<'a>() -> Parser<'a, Expr> {
        let parsed =
            pattern::pattern() - space() - sym(Token::Equal).discard() - space() + expression(true);
        parsed.map(|res| {
            res.into_source()
                .map(|(pat, expr)| Expression::Set(pat.into(), expr.into()))
        })
    }

    /// An unset expression.
    fn unset<'a>() -> Parser<'a, Expr> {
        (word() - space() - sym(Token::Equal).discard()).map(|e| e.map(Expression::Unset))
    }

    /// A nested expression.
    ///
    /// Expressions may be nested with parentheses or indexing.
    fn nested<'a>() -> Parser<'a, Expr> {
        let paren_expr = (sym(Token::OpenParen) - spacenl() + expression(false) - spacenl()
            + sym(Token::CloseParen))
        .map(|res| res.into_source().map(|(e, _)| e.unwrap().1.unwrap()));
        let empty_parens = (sym(Token::OpenParen) + sym(Token::CloseParen))
            .map(|res| res.into_source().with(Expression::Empty));
        let empty_index = sym(Token::Colon).map(|res: Source<_>| res.with(Expression::Empty));

        paren_expr | empty_parens | index(false) | empty_index
    }

    /// An index expression.
    ///
    /// Prior determines whether to try to parse a previous argument value. If true, this may
    /// return a non-index expression (the prior if an index expression is not found).
    fn index<'a>(prior: bool) -> Parser<'a, Expr> {
        let ind = (sym(Token::Colon) + arg_no_index()).map(|res| res.into_source().map(|v| v.1));
        if prior {
            (arg_no_index() + ind.repeat(0..)).map(|(p, i)| {
                i.into_iter().fold(p, |acc, v| {
                    v.map(|v| Expression::Index(Some(acc.into()), v.into()))
                })
            })
        } else {
            ind.map(|v| v.map(|v| Expression::Index(None, v.into())))
        }
    }

    /// An expression in argument position (words are interpreted as strings rather than as commands).
    pub fn arg<'a>() -> Parser<'a, Expr> {
        call(|| index(true))
    }

    fn arg_no_index<'a>() -> Parser<'a, Expr> {
        call(|| nested() | eqword().map(|s| s.map(Expression::String)) | array() | block())
    }

    struct CommandArgs {
        cmd: MergeExpr,
        args: VecDeque<MergeExpr>,
    }

    impl CommandArgs {
        pub fn new<I: IntoIterator<Item = MergeExpr>>(cmd: MergeExpr, vals: I) -> Self {
            CommandArgs {
                cmd,
                args: vals.into_iter().collect(),
            }
        }
    }

    impl IntoSource for CommandArgs {
        type Output = (MergeExpr, Vec<MergeExpr>);

        fn into_source(self) -> Source<Self::Output> {
            (self.cmd, Vec::from(self.args))
                .into_source()
                .map(|(a, b)| (a, b.unwrap()))
        }
    }

    /// Get command arguments.
    fn command_args<'a>() -> Parser<'a, CommandArgs> {
        (merge_with(arg()) + (req_space() * merge_with(arg())).repeat(0..))
            .map(|(first, rest)| CommandArgs::new(first, rest))
    }

    /// Translate command arguments into a command expression.
    ///
    /// If `literal` is true, a lone argument will be returned as itself (rather than a command
    /// with no arguments).
    ///
    /// Specifically, if `literal` is true then:
    /// `something` -> string
    /// `command arg1 ...` -> command
    /// else:
    /// `something` -> no-arg command
    /// `command arg1 ...` -> command
    ///
    /// Returns None if the first argument is a merge expression.
    fn command(cmd: CommandArgs, literal: bool) -> Option<Expr> {
        cmd.into_source()
            .map(move |(cmd, args)| {
                if cmd.merge {
                    return None;
                }
                let cmd = cmd.unwrap().expr;
                let no_args = args.is_empty();
                Some(if no_args && literal {
                    cmd.unwrap()
                } else {
                    Expression::Command(Box::new(cmd), args)
                })
            })
            .transpose()
    }

    /// A command which may contain pipe operators.
    ///
    /// The pipe operators are syntax sugar for nested expressions (and sometimes the order of
    /// expressions, too).
    ///
    /// l |> r -> (l) r, left-associative, low precedence
    /// l <| r -> l (r), right-associative, high precedence
    /// l | r -> r (l), left-associative, low precedence
    fn command_pipes<'a>(literal: bool) -> Parser<'a, Expr> {
        fn merge_expr(e: Expr) -> MergeExpr {
            e.source().with(MergeExpression {
                merge: false,
                expr: e,
            })
        }

        const MERGE_EXPR_ERROR: &'static str = "cannot begin a command with a merge expression";

        fn backward_pipes<'a>() -> Parser<'a, CommandArgs> {
            (command_args()
                + (space() * sym(Token::PipeLeft) * spacenl() * command_args()).repeat(0..))
            .convert(|(first, rest)| {
                if rest.is_empty() {
                    Ok(first)
                } else {
                    let mut iter = rest.into_iter().rev().chain(std::iter::once(first));
                    let first = iter.next().unwrap();
                    iter.fold(Some(first), |cmd, mut v| {
                        v.args.push_back(merge_expr(command(cmd?, true)?));
                        Some(v)
                    })
                    .ok_or(MERGE_EXPR_ERROR)
                }
            })
        }

        let forward_pipes = (backward_pipes()
            + (space() * one_of([Token::Pipe, Token::PipeRight].as_ref())
                + spacenl_present()
                + backward_pipes())
            .repeat(0..))
        .convert(|(first, rest)| {
            if rest.is_empty() {
                Ok(first)
            } else {
                rest.into_iter()
                    .fold(Some(first), |cmd, ((tp, ws), mut v)| {
                        let cmd = command(cmd?, true)?;
                        Some(if tp == Token::Pipe {
                            v.args.push_back(merge_expr(cmd));
                            v
                        } else {
                            debug_assert!(tp == Token::PipeRight);

                            // If no whitespace, check whether the first expression is an index
                            // expression without prior, and if so index into the command.
                            let indexed_cmd = if !ws {
                                if let MergeExpression {
                                    merge: false,
                                    ref mut expr,
                                } = &mut *v.cmd
                                {
                                    let mut expr: &mut Expr = expr;
                                    loop {
                                        if let Expression::Index(ref mut a, _) = &mut **expr {
                                            if let Some(v) = a {
                                                expr = &mut *v;
                                            } else {
                                                *a = Some(cmd.into());
                                                break Ok(v.cmd);
                                            }
                                        } else {
                                            break Err((v.cmd, cmd));
                                        }
                                    }
                                } else {
                                    Err((v.cmd, cmd))
                                }
                            } else {
                                Err((v.cmd, cmd))
                            };

                            let mut args = v.args;

                            let cmd = indexed_cmd.unwrap_or_else(|(arg, cmd)| {
                                args.push_front(arg);
                                merge_expr(cmd)
                            });

                            CommandArgs { cmd, args }
                        })
                    })
                    .ok_or(MERGE_EXPR_ERROR)
            }
        });

        forward_pipes.convert(move |args| command(args, literal).ok_or(MERGE_EXPR_ERROR))
    }

    /// A function expression.
    fn function<'a>() -> Parser<'a, Expr> {
        let tok = tag("fn");
        let end_tok = function_delim();
        let pat = req_space() * pattern::command_pattern() - req_space() - end_tok;
        let rest = space() * expression(true);
        (tok + pat + rest.expect("fn argument")).map(|res| {
            let p = res.into_source();
            p.map(|(pat, e)| {
                let (_, pat) = pat.unwrap();
                Expression::Function(pat.into(), e.into())
            })
        })
    }

    /// A match expression.
    fn match_expr<'a>() -> Parser<'a, Expr> {
        let tok = tag("match");
        let item = pattern::pattern() - space() - sym(Token::Equal) - space() + expression(true);
        let rest = req_space() * arg() - spacenl() + block_syntax(item);
        (tok + rest.expect("match arguments")).map(|res| {
            res.into_source().map(|(_, vs)| {
                let (val, pats) = vs.unwrap();
                Expression::Match(
                    val.into(),
                    pats.unwrap().into_iter().map(Source::unwrap).collect(),
                )
            })
        })
    }

    /// An array expression.
    fn array<'a>() -> Parser<'a, Expr> {
        array_syntax(merge_with(expression(true))).map(|e| e.into_source().map(Expression::Array))
    }

    /// A block expression.
    fn block<'a>() -> Parser<'a, Expr> {
        // Change literal strings in blocks to set the string from the currently-bound value
        // i.e., { a } -> { a = :a }
        let item = expression(true).map(|e| {
            let (source, e) = e.take();
            source.clone().with(match e {
                Expression::String(s) => Expression::Set(
                    source.clone().with(Pattern::Binding(s.clone())).into(),
                    source
                        .clone()
                        .with(Expression::Index(
                            None,
                            source.with(Expression::String(s)).into(),
                        ))
                        .into(),
                ),
                other => other,
            })
        });
        block_syntax(merge_with(item)).map(|e| e.into_source().map(Expression::Block))
    }

    /// A merge expression generator.
    pub fn merge_with<'a>(p: Parser<'a, Expr>) -> Parser<'a, MergeExpr> {
        (sym(Token::Caret) + arg()).map(|es| {
            es.into_source().map(|(_, e)| MergeExpression {
                merge: true,
                expr: e,
            })
        }) | p.map(|e| {
            let src = e.source();
            src.with(MergeExpression {
                merge: false,
                expr: e,
            })
        })
    }

    /// Expressions with an opening keyword.
    pub fn kw_expr<'a>() -> Parser<'a, Expr> {
        function() | match_expr()
    }

    /// A single expression.
    pub fn expression<'a>(literal: bool) -> Parser<'a, Expr> {
        call(move || kw_expr() | set() | unset() | command_pipes(literal))
    }
}

/// Parses array syntax, with an inner value parser.
fn array_syntax<'a, T: 'a>(inner: Parser<'a, T>) -> Parser<'a, Vec<T>> {
    sym(Token::OpenBracket) * spacenl() * list(inner, delim())
        - spacenl()
        - sym(Token::CloseBracket)
}

/// Parses block syntax, with an inner value parser.
fn block_syntax<'a, T: 'a>(inner: Parser<'a, T>) -> Parser<'a, Vec<T>> {
    sym(Token::OpenCurly) * spacenl() * list(inner, delim()) - spacenl() - sym(Token::CloseCurly)
}

/// A single string.
fn word<'a>() -> Parser<'a, Source<String>> {
    Parser::new(|a, s| {
        if let Some(t) = a.get(s) {
            if let Token::String(st) = &**t {
                Ok((t.clone().with(st.clone()), s + 1))
            } else {
                Err(pom::Error::Mismatch {
                    message: format!("expected string, found {}", t),
                    position: s,
                })
            }
        } else {
            Err(pom::Error::Incomplete)
        }
    })
}

/// A single string, allowing equals signs.
fn eqword<'a>() -> Parser<'a, Source<String>> {
    (word() | sym(Token::Equal).map(|s: Source<Token>| s.with("=".to_owned())))
        .repeat(1..)
        .map(|ss| {
            ss.into_source()
                .map(|s| String::from_iter(s.into_iter().map(|s| s.unwrap())))
        })
}

/// A string token with the given contents.
fn tag<'a>(s: &'a str) -> Parser<'a, Source<()>> {
    let tok = Token::String(s.to_string());
    is_a(move |v| v == tok).map(|e: Tok| e.with(()))
}

/// A required whitespace.
fn req_space<'a>() -> Parser<'a, ()> {
    sym(Token::Whitespace).discard()
}

/// An optional whitespace.
fn space<'a>() -> Parser<'a, ()> {
    sym(Token::Whitespace).opt().discard()
}

/// Zero or more whitespace characters, including newlines.
fn spacenl<'a>() -> Parser<'a, ()> {
    spacenl_present().discard()
}

/// Zero or more whitespace characters, including newlines.
/// Returns whether whitespace was present or not.
fn spacenl_present<'a>() -> Parser<'a, bool> {
    one_of([Token::Newline, Token::Whitespace].as_ref())
        .repeat(0..)
        .map(|v| !v.is_empty())
}

/// A separating delimiter.
fn delim<'a>() -> Parser<'a, ()> {
    space() * one_of([Token::Newline, Token::Comma, Token::Semicolon].as_ref()) * spacenl()
}

/// A function parameter delimiter.
fn function_delim<'a>() -> Parser<'a, Source<()>> {
    tag("->")
}

#[cfg(test)]
mod test {
    use super::expression;
    use super::pattern;
    use super::*;
    use Token::*;

    type Result = pom::Result<()>;

    fn src<T>(e: T) -> Source<T> {
        Source::new(NoSource).with(e)
    }

    fn assert_parse<T: PartialEq<I> + std::fmt::Debug, I>(
        s: &[Token],
        parser: impl for<'a> FnOnce(std::marker::PhantomData<&'a ()>) -> Parser<'a, T>,
        expected: I,
    ) -> Result {
        let src = Source::new(NoSource);
        let toks: Vec<_> = s
            .into_iter()
            .cloned()
            .map(|tok| src.clone().with(tok))
            .collect();
        let r = parser(std::marker::PhantomData).parse(&toks)?;
        dbg!(&r);
        assert!(r == expected);
        Ok(())
    }

    #[test]
    fn word() -> Result {
        assert_parse(
            &[String("hello".to_owned())],
            |_| super::word(),
            "hello".to_owned(),
        )
    }

    mod pattern_test {
        use super::pattern::*;
        use super::*;

        fn assert(s: &[Token], expected: Pattern<Expr>) -> Result {
            assert_parse(s, |_| pattern(), expected)
        }

        #[test]
        fn any() -> Result {
            assert(&[String("_".into())], Pattern::Any)
        }

        #[test]
        fn literal() -> Result {
            assert(
                &[Equal, String("howdy".into())],
                Pattern::Literal(src(Expression::String("howdy".into()))),
            )
        }

        #[test]
        fn binding() -> Result {
            assert(&[String("a".into())], Pattern::Binding("a".into()))
        }

        #[test]
        fn array() -> Result {
            assert(
                &[
                    OpenBracket,
                    String("a".into()),
                    Comma,
                    String("b".into()),
                    Comma,
                    Caret,
                    String("rest".into()),
                    CloseBracket,
                ],
                Pattern::Array(vec![
                    src(ArrayPattern::Item(src(Pattern::Binding("a".into())))),
                    src(ArrayPattern::Item(src(Pattern::Binding("b".into())))),
                    src(ArrayPattern::Rest(src(Pattern::Binding("rest".into())))),
                ]),
            )
        }

        #[test]
        fn map() -> Result {
            assert(
                &[
                    OpenCurly,
                    String("a".into()),
                    Comma,
                    String("b".into()),
                    Equal,
                    Equal,
                    String("c".into()),
                    Comma,
                    Caret,
                    String("rest".into()),
                    CloseCurly,
                ],
                Pattern::Map(vec![
                    src(MapPattern::Item(
                        "a".into(),
                        src(Pattern::Binding("a".into())),
                    )),
                    src(MapPattern::Item(
                        "b".into(),
                        src(Pattern::Literal(src(Expression::String("c".into())))),
                    )),
                    src(MapPattern::Rest(src(Pattern::Binding("rest".into())))),
                ]),
            )
        }

        #[test]
        fn command() -> Result {
            assert_parse(
                &[
                    Caret,
                    OpenCurly,
                    Caret,
                    String("keys".into()),
                    CloseCurly,
                    Whitespace,
                    String("a".into()),
                    Whitespace,
                    Caret,
                    String("rest".into()),
                ],
                |_| command_pattern(),
                src(vec![
                    src(ArrayPattern::Rest(src(Pattern::Map(vec![src(
                        MapPattern::Rest(src(Pattern::Binding("keys".into()))),
                    )])))),
                    src(ArrayPattern::Item(src(Pattern::Binding("a".into())))),
                    src(ArrayPattern::Rest(src(Pattern::Binding("rest".into())))),
                ]),
            )
        }
    }

    mod expression_test {
        use super::expression::*;
        use super::*;

        fn nomerge(e: Expression) -> Source<MergeExpression> {
            src(MergeExpression {
                merge: false,
                expr: src(e),
            })
        }

        fn merge(e: Expression) -> Source<MergeExpression> {
            src(MergeExpression {
                merge: true,
                expr: src(e),
            })
        }

        fn pat(s: &str) -> Box<Pat> {
            src(Pattern::Binding(s.into())).into()
        }

        fn assert(s: &[Token], expected: Expression) -> Result {
            assert_parse(s, |_| expression(true), expected)
        }

        #[test]
        fn string() -> Result {
            assert(
                &[String("str".to_owned())],
                Expression::String("str".to_owned()),
            )
        }

        #[test]
        fn command() -> Result {
            assert(
                &[
                    String("echo".to_owned()),
                    Whitespace,
                    String("howdy".to_owned()),
                ],
                Expression::Command(
                    Box::new(src(Expression::String("echo".to_owned()))),
                    vec![nomerge(Expression::String("howdy".to_owned()))],
                ),
            )
        }

        #[test]
        fn command_no_args() -> Result {
            assert(
                &[OpenParen, String("f".into()), CloseParen],
                Expression::Command(src(Expression::String("f".into())).into(), vec![]),
            )?;
            assert(
                &[
                    OpenParen,
                    String("m".into()),
                    Colon,
                    String("f".into()),
                    CloseParen,
                ],
                Expression::Command(
                    src(Expression::Index(
                        Some(src(Expression::String("m".into())).into()),
                        src(Expression::String("f".into())).into(),
                    ))
                    .into(),
                    vec![],
                ),
            )?;
            Ok(())
        }

        #[test]
        fn pipe_commands() -> Result {
            assert(
                &[
                    String("e".into()),
                    Pipe,
                    String("d".into()),
                    Pipe,
                    String("a".into()),
                    Whitespace,
                    String("b".into()),
                    Whitespace,
                    String("c".into()),
                ],
                Expression::Command(
                    src(Expression::String("a".into())).into(),
                    vec![
                        nomerge(Expression::String("b".into())),
                        nomerge(Expression::String("c".into())),
                        nomerge(Expression::Command(
                            src(Expression::String("d".into())).into(),
                            vec![nomerge(Expression::String("e".into()))],
                        )),
                    ],
                ),
            )
        }

        #[test]
        fn pipe_right_commands() -> Result {
            assert(
                &[
                    String("a".into()),
                    Whitespace,
                    String("b".into()),
                    PipeRight,
                    String("c".into()),
                    PipeRight,
                    String("d".into()),
                    Whitespace,
                    String("e".into()),
                ],
                Expression::Command(
                    src(Expression::Command(
                        src(Expression::Command(
                            src(Expression::String("a".into())).into(),
                            vec![nomerge(Expression::String("b".into()))],
                        ))
                        .into(),
                        vec![nomerge(Expression::String("c".into()))],
                    ))
                    .into(),
                    vec![
                        nomerge(Expression::String("d".into())),
                        nomerge(Expression::String("e".into())),
                    ],
                ),
            )
        }

        #[test]
        fn pipe_left_commands() -> Result {
            assert(
                &[
                    String("a".into()),
                    Whitespace,
                    String("b".into()),
                    PipeLeft,
                    String("c".into()),
                    PipeLeft,
                    String("d".into()),
                    Whitespace,
                    String("e".into()),
                ],
                Expression::Command(
                    src(Expression::String("a".into())).into(),
                    vec![
                        nomerge(Expression::String("b".into())),
                        nomerge(Expression::Command(
                            src(Expression::String("c".into())).into(),
                            vec![nomerge(Expression::Command(
                                src(Expression::String("d".into())).into(),
                                vec![nomerge(Expression::String("e".into()))],
                            ))],
                        )),
                    ],
                ),
            )
        }

        #[test]
        fn pipe_commands_precedence() -> Result {
            // a | b <| c <| d | e |> f <| g | h
            assert(
                &[
                    String("a".into()),
                    Pipe,
                    String("b".into()),
                    PipeLeft,
                    String("c".into()),
                    PipeLeft,
                    String("d".into()),
                    Pipe,
                    String("e".into()),
                    PipeRight,
                    String("f".into()),
                    PipeLeft,
                    String("g".into()),
                    Pipe,
                    String("h".into()),
                ],
                Expression::Command(
                    src(Expression::String("h".into())).into(),
                    vec![nomerge(Expression::Command(
                        src(Expression::Command(
                            src(Expression::String("e".into())).into(),
                            vec![nomerge(Expression::Command(
                                src(Expression::String("b".into())).into(),
                                vec![
                                    nomerge(Expression::Command(
                                        src(Expression::String("c".into())).into(),
                                        vec![nomerge(Expression::String("d".into()))],
                                    )),
                                    nomerge(Expression::String("a".into())),
                                ],
                            ))],
                        ))
                        .into(),
                        vec![
                            nomerge(Expression::String("f".into())),
                            nomerge(Expression::String("g".into())),
                        ],
                    ))],
                ),
            )
        }

        #[test]
        fn pipe_indexing() -> Result {
            assert(
                &[
                    String("a".into()),
                    PipeRight,
                    Whitespace,
                    Colon,
                    String("b".into()),
                ],
                Expression::Command(
                    src(Expression::String("a".into())).into(),
                    vec![nomerge(Expression::Index(
                        None,
                        src(Expression::String("b".into())).into(),
                    ))],
                ),
            )?;

            assert(
                &[String("a".into()), PipeRight, Colon, String("b".into())],
                Expression::Index(
                    Some(src(Expression::String("a".into())).into()),
                    src(Expression::String("b".into())).into(),
                ),
            )?;
            Ok(())
        }

        #[test]
        #[ignore = "not yet supported"]
        fn pipe_keywords() -> Result {
            assert(
                &[
                    String("a".into()),
                    Whitespace,
                    PipeLeft,
                    Whitespace,
                    String("match".into()),
                    Whitespace,
                    String("b".into()),
                    Whitespace,
                    OpenCurly,
                    CloseCurly,
                ],
                Expression::Command(
                    src(Expression::String("a".into())).into(),
                    vec![nomerge(Expression::Match(
                        src(Expression::String("b".into())).into(),
                        vec![],
                    ))],
                ),
            )?;

            assert(
                &[
                    String("a".into()),
                    Whitespace,
                    PipeLeft,
                    Whitespace,
                    String("fn".into()),
                    Whitespace,
                    String("b".into()),
                    Whitespace,
                    String("->".into()),
                    Whitespace,
                    OpenCurly,
                    CloseCurly,
                ],
                Expression::Command(
                    src(Expression::String("a".into())).into(),
                    vec![nomerge(Expression::Function(
                        src(vec![src(ArrayPattern::Item(src(Pattern::Binding(
                            "b".into(),
                        ))))]),
                        src(Expression::Block(vec![])).into(),
                    ))],
                ),
            )?;

            Ok(())
        }

        #[test]
        fn args() -> Result {
            assert_parse(
                &[Colon, String("howdy".to_owned())],
                |_| arg(),
                Expression::Index(None, Box::new(src(Expression::String("howdy".to_owned())))),
            )?;
            assert_parse(
                &[
                    OpenParen,
                    String("hello".to_owned()),
                    Whitespace,
                    String("world".to_owned()),
                    CloseParen,
                ],
                |_| arg(),
                Expression::Command(
                    Box::new(src(Expression::String("hello".to_owned()))),
                    vec![nomerge(Expression::String("world".to_owned()))],
                ),
            )?;
            assert_parse(
                &[
                    OpenCurly,
                    Whitespace,
                    String("a".into()),
                    Equal,
                    String("ls".into()),
                    CloseCurly,
                ],
                |_| arg(),
                Expression::Block(vec![nomerge(Expression::Set(
                    pat("a"),
                    src(Expression::String("ls".into())).into(),
                ))]),
            )?;
            assert_parse(
                &[
                    OpenBracket,
                    Newline,
                    OpenParen,
                    String("a".into()),
                    CloseParen,
                    Newline,
                    Colon,
                    String("b".into()),
                    Newline,
                    CloseBracket,
                ],
                |_| arg(),
                Expression::Array(vec![
                    nomerge(Expression::Command(
                        src(Expression::String("a".into())).into(),
                        vec![],
                    )),
                    nomerge(Expression::Index(
                        None,
                        src(Expression::String("b".into())).into(),
                    )),
                ]),
            )?;
            Ok(())
        }

        #[test]
        fn empty() -> Result {
            assert(&[Colon], Expression::Empty)?;
            assert(&[OpenParen, CloseParen], Expression::Empty)?;
            Ok(())
        }

        #[test]
        fn array() -> Result {
            assert(
                &[
                    OpenBracket,
                    Whitespace,
                    String("a".to_owned()),
                    Whitespace,
                    Comma,
                    String("b".to_owned()),
                    Whitespace,
                    Semicolon,
                    Newline,
                    String("c".to_owned()),
                    Newline,
                    String("d".to_owned()),
                    Whitespace,
                    CloseBracket,
                ],
                Expression::Array(vec![
                    nomerge(Expression::String("a".into())).into(),
                    nomerge(Expression::String("b".into())).into(),
                    nomerge(Expression::String("c".into())).into(),
                    nomerge(Expression::String("d".into())).into(),
                ]),
            )
        }

        #[test]
        fn set() -> Result {
            assert(
                &[
                    String("a".to_owned()),
                    Equal,
                    OpenParen,
                    String("echo".to_owned()),
                    CloseParen,
                ],
                Expression::Set(
                    pat("a"),
                    src(Expression::Command(
                        src(Expression::String("echo".to_owned())).into(),
                        vec![],
                    ))
                    .into(),
                ),
            )?;
            assert(
                &[String("a".to_owned()), Equal, String("a".to_owned())],
                Expression::Set(pat("a"), src(Expression::String("a".into())).into()),
            )?;
            Ok(())
        }

        #[test]
        fn unset() -> Result {
            assert(
                &[String("a".to_owned()), Equal],
                Expression::Unset("a".to_owned()),
            )
        }

        #[test]
        fn block() -> Result {
            assert(
                &[
                    OpenCurly,
                    Whitespace,
                    String("a".to_owned()),
                    Newline,
                    Whitespace,
                    Colon,
                    Whitespace,
                    Newline,
                    String("c".to_owned()),
                    Whitespace,
                    Equal,
                    Whitespace,
                    OpenParen,
                    String("echo".to_owned()),
                    CloseParen,
                    Newline,
                    CloseCurly,
                ],
                Expression::Block(vec![
                    nomerge(Expression::Set(
                        pat("a"),
                        src(Expression::Index(
                            None,
                            src(Expression::String("a".into())).into(),
                        ))
                        .into(),
                    )),
                    nomerge(Expression::Empty),
                    nomerge(Expression::Set(
                        pat("c"),
                        Box::new(src(Expression::Command(
                            Box::new(src(Expression::String("echo".to_owned()))),
                            vec![],
                        ))),
                    )),
                ]),
            )
        }

        #[test]
        fn function() -> Result {
            assert(
                &[
                    String("fn".into()),
                    Whitespace,
                    Caret,
                    String("_".into()),
                    Whitespace,
                    String("->".into()),
                    Whitespace,
                    String("howdy".into()),
                ],
                Expression::Function(
                    src(vec![src(ArrayPattern::Rest(src(Pattern::Any)))]),
                    src(Expression::String("howdy".into())).into(),
                ),
            )
        }

        #[test]
        fn index() -> Result {
            assert(
                &[
                    OpenBracket,
                    String("a".into()),
                    Comma,
                    String("b".into()),
                    CloseBracket,
                    Colon,
                    String("0".into()),
                ],
                Expression::Index(
                    Some(
                        src(Expression::Array(vec![
                            nomerge(Expression::String("a".into())).into(),
                            nomerge(Expression::String("b".into())).into(),
                        ]))
                        .into(),
                    ),
                    src(Expression::String("0".into())).into(),
                ),
            )
        }

        #[test]
        fn multi_index() -> Result {
            assert(
                &[
                    String("a".into()),
                    Colon,
                    String("b".into()),
                    Colon,
                    String("c".into()),
                ],
                Expression::Index(
                    Some(
                        src(Expression::Index(
                            Some(src(Expression::String("a".into())).into()),
                            src(Expression::String("b".into())).into(),
                        ))
                        .into(),
                    ),
                    src(Expression::String("c".into())).into(),
                ),
            )
        }

        #[test]
        fn merge_array() -> Result {
            assert(
                &[
                    OpenBracket,
                    Caret,
                    OpenBracket,
                    String("b".into()),
                    CloseBracket,
                    CloseBracket,
                ],
                Expression::Array(vec![merge(Expression::Array(vec![nomerge(
                    Expression::String("b".into()),
                )
                .into()]))]),
            )
        }

        #[test]
        fn merge_block() -> Result {
            assert(
                &[
                    OpenCurly,
                    Caret,
                    OpenCurly,
                    String("a".into()),
                    Equal,
                    String("b".into()),
                    CloseCurly,
                    CloseCurly,
                ],
                Expression::Block(vec![merge(Expression::Block(vec![nomerge(
                    Expression::Set(pat("a"), src(Expression::String("b".into())).into()),
                )]))]),
            )
        }

        #[test]
        fn merge_command() -> Result {
            assert(
                &[
                    String("a".into()),
                    Whitespace,
                    Caret,
                    OpenBracket,
                    String("b".into()),
                    CloseBracket,
                    Whitespace,
                    Caret,
                    OpenCurly,
                    String("c".into()),
                    Equal,
                    String("d".into()),
                    CloseCurly,
                ],
                Expression::Command(
                    src(Expression::String("a".into())).into(),
                    vec![
                        merge(Expression::Array(vec![nomerge(Expression::String(
                            "b".into(),
                        ))])),
                        merge(Expression::Block(vec![nomerge(Expression::Set(
                            pat("c"),
                            src(Expression::String("d".into())).into(),
                        ))])),
                    ],
                ),
            )
        }
    }
}
