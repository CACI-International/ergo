//! AST parsing.

use super::tokenize::{Tok, Token};
use super::*;
use pom::parser::*;
use std::iter::FromIterator;

/// A parser alias.
pub type Parser<'a, T> = pom::parser::Parser<'a, Tok, T>;

/// Return a parser for a script.
pub fn script<'a>() -> Parser<'a, Script> {
    spacenl() * list(merge_with(expression()), eoe()).map(|e| e.into_source()) - spacenl() - end()
}

/// A set variable expression.
fn set_variable<'a>() -> Parser<'a, Expr> {
    let parsed = word() - space() - sym(Token::Equal).discard() - space() + extarg();
    parsed.map(|res| {
        res.into_source()
            .map(|(var, expr)| Expression::SetVariable(var.unwrap(), expr.into()))
    })
}

/// An unset variable expression.
fn unset_variable<'a>() -> Parser<'a, Expr> {
    (word() - space() - sym(Token::Equal).discard()).map(|e| e.map(Expression::UnsetVariable))
}

/// A nested expression.
///
/// Expressions may be nested with parentheses or the shorthand $expr.
fn nested<'a>() -> Parser<'a, Expr> {
    let word_nested = is_a(|t: Tok| match t.as_ref() {
        Token::String(_) => true,
        _ => false,
    })
    .repeat(1..)
    .convert(|e| expression().parse(&e));
    let self_nested =
        call(|| nested()).map(|e| e.clone().with(Expression::Command(e.into(), vec![])));

    let paren_expr =
        sym(Token::OpenParen) * spacenl() * expression() - spacenl() - sym(Token::CloseParen);
    let empty_parens = (sym(Token::OpenParen) + sym(Token::CloseParen))
        .map(|res| res.into_source().with(Expression::Empty));
    let dollar = sym(Token::Dollar) * (word_nested | array() | block() | self_nested);
    let empty_dollar = sym(Token::Dollar).map(|res: Source<_>| res.with(Expression::Empty));

    paren_expr | empty_parens | dollar | empty_dollar
}

/// An expression in argument position (words are interpreted as strings rather than as commands).
fn arg<'a>() -> Parser<'a, Expr> {
    call(|| nested() | eqword().map(|s| s.map(Expression::String)) | array() | block())
}

/// An expression in argument position, or special expressions (if and fn)
fn extarg<'a>() -> Parser<'a, Expr> {
    function() | if_expr() | arg()
}

/// A command expression.
fn command<'a>() -> Parser<'a, Expr> {
    (arg() + (req_space() * list(merge_with(arg()), req_space())).opt()).map(|res| {
        res.into_source().map(|(cmd, args)| {
            Expression::Command(
                Box::new(cmd),
                args.unwrap().map(|e| e.unwrap()).unwrap_or(vec![]),
            )
        })
    })
}

/// A function expression.
fn function<'a>() -> Parser<'a, Expr> {
    let tok = tag("fn");
    let rest = req_space() * call(|| extarg());
    (tok + rest.expect("fn argument")).map(|res| {
        let p: Source<_> = res.into_source();
        p.map(|(_, e)| Expression::Function(Box::new(e)))
    })
}

/// An if expression.
fn if_expr<'a>() -> Parser<'a, Expr> {
    let tok = tag("if");
    let rest = req_space() * arg() - req_space() + arg() - req_space() + arg();
    //.map(|((cond, t), f)| Expression::If(Box::new(cond), Box::new(t), Box::new(f)));
    (tok + rest.expect("if arguments")).map(|res| {
        let p: Source<_> = res.into_source();
        p.map(|(_, vals)| {
            let (condt, f) = vals.unwrap();
            let (cond, t) = condt.unwrap();
            Expression::If(cond.into(), t.into(), f.into())
        })
    })
}

/// An array expression.
fn array<'a>() -> Parser<'a, Expr> {
    sym(Token::OpenBracket)
        * spacenl()
        * list(merge_with(arg()), eoe()).map(|e| e.into_source().map(Expression::Array))
        - spacenl()
        - sym(Token::CloseBracket)
}

/// A block expression.
fn block<'a>() -> Parser<'a, Expr> {
    sym(Token::OpenCurly)
        * spacenl()
        * list(merge_with(expression()), eoe()).map(|e| e.into_source().map(Expression::Block))
        - spacenl()
        - sym(Token::CloseCurly)
}

/// A single expression.
fn expression<'a>() -> Parser<'a, Expr> {
    call(|| function() | if_expr() | set_variable() | unset_variable() | command())
}

/// A merge expression generator.
fn merge_with<'a>(p: Parser<'a, Expr>) -> Parser<'a, MergeExpr> {
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

/// A single string.
fn word<'a>() -> Parser<'a, Source<String>> {
    Parser::new(|a, s| {
        if let Some(t) = a.get(s) {
            if let Token::String(st) = t.as_ref() {
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
    one_of([Token::Newline, Token::Whitespace].as_ref())
        .repeat(0..)
        .discard()
}

/// End of expression delimiter.
fn eoe<'a>() -> Parser<'a, ()> {
    space() * one_of([Token::Newline, Token::Comma, Token::Semicolon].as_ref()) * spacenl()
}

#[cfg(test)]
mod test {
    use super::*;
    use Token::*;

    type Result = pom::Result<()>;

    fn src<T>(e: T) -> Source<T> {
        Source::new(NoSource).with(e)
    }

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

    #[test]
    fn word() -> Result {
        assert_parse(
            &[Token::String("hello".to_owned())],
            |_| super::word(),
            "hello".to_owned(),
        )
    }

    #[test]
    fn command() -> Result {
        assert_expr(
            &[
                Token::String("echo".to_owned()),
                Whitespace,
                Token::String("howdy".to_owned()),
            ],
            Expression::Command(
                Box::new(src(Expression::String("echo".to_owned()))),
                vec![nomerge(Expression::String("howdy".to_owned()))],
            ),
        )
    }

    #[test]
    fn arg() -> Result {
        assert_parse(
            &[Dollar, Token::String("howdy".to_owned())],
            |_| super::arg(),
            Expression::Command(
                Box::new(src(Expression::String("howdy".to_owned()))),
                vec![],
            ),
        )?;
        assert_parse(
            &[
                OpenParen,
                Token::String("hello".to_owned()),
                Whitespace,
                Token::String("world".to_owned()),
                CloseParen,
            ],
            |_| super::arg(),
            Expression::Command(
                Box::new(src(Expression::String("hello".to_owned()))),
                vec![nomerge(Expression::String("world".to_owned()))],
            ),
        )?;
        assert_parse(
            &[
                OpenCurly,
                Whitespace,
                Token::String("a".into()),
                Equal,
                Token::String("ls".into()),
                CloseCurly,
            ],
            |_| super::arg(),
            Expression::Block(vec![nomerge(Expression::SetVariable(
                "a".into(),
                src(Expression::String("ls".into())).into(),
            ))]),
        )?;
        assert_parse(
            &[
                OpenBracket,
                Newline,
                OpenParen,
                Token::String("a".into()),
                CloseParen,
                Newline,
                OpenParen,
                Token::String("b".into()),
                CloseParen,
                Newline,
                CloseBracket,
            ],
            |_| super::arg(),
            Expression::Array(vec![
                nomerge(Expression::Command(
                    src(Expression::String("a".into())).into(),
                    vec![],
                )),
                nomerge(Expression::Command(
                    src(Expression::String("b".into())).into(),
                    vec![],
                )),
            ]),
        )
    }

    #[test]
    fn empty() -> Result {
        assert_expr(
            &[Dollar],
            Expression::Command(src(Expression::Empty).into(), vec![]),
        )?;
        assert_expr(
            &[OpenParen, CloseParen],
            Expression::Command(src(Expression::Empty).into(), vec![]),
        )
    }

    #[test]
    fn array() -> Result {
        assert_expr(
            &[
                OpenBracket,
                Whitespace,
                Token::String("a".to_owned()),
                Whitespace,
                Comma,
                Token::String("b".to_owned()),
                Whitespace,
                Semicolon,
                Newline,
                Token::String("c".to_owned()),
                Newline,
                Token::String("d".to_owned()),
                Whitespace,
                CloseBracket,
            ],
            Expression::Command(
                src(Expression::Array(vec![
                    nomerge(Expression::String("a".into())).into(),
                    nomerge(Expression::String("b".into())).into(),
                    nomerge(Expression::String("c".into())).into(),
                    nomerge(Expression::String("d".into())).into(),
                ]))
                .into(),
                vec![],
            ),
        )
    }

    #[test]
    fn set_variable() -> Result {
        assert_expr(
            &[
                Token::String("a".to_owned()),
                Token::Equal,
                Token::OpenParen,
                Token::String("echo".to_owned()),
                Token::CloseParen,
            ],
            Expression::SetVariable(
                "a".to_owned(),
                src(Expression::Command(
                    src(Expression::String("echo".to_owned())).into(),
                    vec![],
                ))
                .into(),
            ),
        )
    }

    #[test]
    fn unset_variable() -> Result {
        assert_expr(
            &[Token::String("a".to_owned()), Token::Equal],
            Expression::UnsetVariable("a".to_owned()),
        )
    }

    #[test]
    fn block() -> Result {
        assert_expr(
            &[
                Token::OpenCurly,
                Token::Whitespace,
                Token::String("a".to_owned()),
                Token::Newline,
                Token::Whitespace,
                Token::String("b".to_owned()),
                Token::Whitespace,
                Token::Newline,
                Token::String("c".to_owned()),
                Token::Whitespace,
                Token::Equal,
                Token::Whitespace,
                Token::OpenParen,
                Token::String("echo".to_owned()),
                Token::CloseParen,
                Token::Newline,
                Token::CloseCurly,
            ],
            Expression::Command(
                src(Expression::Block(vec![
                    nomerge(Expression::Command(
                        Box::new(src(Expression::String("a".to_owned()))),
                        vec![],
                    )),
                    nomerge(Expression::Command(
                        Box::new(src(Expression::String("b".to_owned()))),
                        vec![],
                    )),
                    nomerge(Expression::SetVariable(
                        "c".to_owned(),
                        Box::new(src(Expression::Command(
                            Box::new(src(Expression::String("echo".to_owned()))),
                            vec![],
                        ))),
                    )),
                ]))
                .into(),
                vec![],
            ),
        )
    }

    #[test]
    fn function() -> Result {
        assert_expr(
            &[
                Token::String("fn".to_owned()),
                Token::Whitespace,
                Token::String("howdy".to_owned()),
            ],
            Expression::Function(src(Expression::String("howdy".to_owned())).into()),
        )
    }

    #[test]
    fn ifexpr() -> Result {
        assert_expr(
            &[
                Token::String("if".to_owned()),
                Token::Whitespace,
                Token::Dollar,
                Token::String("a".to_owned()),
                Token::Whitespace,
                Token::Dollar,
                Token::String("b".to_owned()),
                Token::Whitespace,
                Token::Dollar,
                Token::String("c".to_owned()),
            ],
            Expression::If(
                Box::new(src(Expression::Command(
                    Box::new(src(Expression::String("a".to_owned()))),
                    vec![],
                ))),
                Box::new(src(Expression::Command(
                    Box::new(src(Expression::String("b".to_owned()))),
                    vec![],
                ))),
                Box::new(src(Expression::Command(
                    Box::new(src(Expression::String("c".to_owned()))),
                    vec![],
                ))),
            ),
        )
    }

    #[test]
    fn index_call() -> Result {
        assert_expr(
            &[
                OpenBracket,
                Token::String("a".into()),
                Comma,
                Token::String("b".into()),
                CloseBracket,
                Whitespace,
                Token::String("0".into()),
            ],
            Expression::Command(
                src(Expression::Array(vec![
                    nomerge(Expression::String("a".into())).into(),
                    nomerge(Expression::String("b".into())).into(),
                ]))
                .into(),
                vec![nomerge(Expression::String("0".into())).into()],
            ),
        )
    }

    #[test]
    fn merge_array() -> Result {
        assert_expr(
            &[
                OpenBracket,
                Caret,
                OpenBracket,
                Token::String("b".into()),
                CloseBracket,
                CloseBracket,
            ],
            Expression::Command(
                src(Expression::Array(vec![merge(Expression::Array(vec![
                    nomerge(Expression::String("b".into())).into(),
                ]))]))
                .into(),
                vec![],
            ),
        )
    }

    #[test]
    fn merge_block() -> Result {
        assert_expr(
            &[
                OpenCurly,
                Caret,
                OpenCurly,
                Token::String("a".into()),
                Equal,
                Token::String("b".into()),
                CloseCurly,
                CloseCurly,
            ],
            Expression::Command(
                src(Expression::Block(vec![merge(Expression::Block(vec![
                    nomerge(Expression::SetVariable(
                        "a".into(),
                        src(Expression::String("b".into())).into(),
                    )),
                ]))]))
                .into(),
                vec![],
            ),
        )
    }

    #[test]
    fn merge_command() -> Result {
        assert_expr(
            &[
                Token::String("a".into()),
                Whitespace,
                Caret,
                OpenBracket,
                Token::String("b".into()),
                CloseBracket,
                Whitespace,
                Caret,
                OpenCurly,
                Token::String("c".into()),
                Equal,
                Token::String("d".into()),
                CloseCurly,
            ],
            Expression::Command(
                src(Expression::String("a".into())).into(),
                vec![
                    merge(Expression::Array(vec![nomerge(Expression::String(
                        "b".into(),
                    ))])),
                    merge(Expression::Block(vec![nomerge(Expression::SetVariable(
                        "c".into(),
                        src(Expression::String("d".into())).into(),
                    ))])),
                ],
            ),
        )
    }

    fn assert_expr(s: &[Token], expected: Expression) -> Result {
        assert_parse(s, |_| expression(), expected)
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
}
