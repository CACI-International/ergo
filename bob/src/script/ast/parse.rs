//! AST parsing.

use super::tokenize::Token;
use super::*;
use pom::parser::*;

/// A parser alias.
pub type Parser<'a, T> = pom::parser::Parser<'a, Token, T>;

/// Return a parser for a script.
pub fn script<'a>() -> Parser<'a, Script> {
    exprs()
}

/// A set variable expression.
fn set_variable<'a>() -> Parser<'a, Expression> {
    let parsed = word() - space() - sym(Token::Equal).discard() - space() + arg();
    parsed.map(|(var, expr)| Expression::SetVariable(var, Box::new(expr)))
}

/// An unset variable expression.
fn unset_variable<'a>() -> Parser<'a, Expression> {
    (word() - space() - sym(Token::Equal).discard()).map(Expression::UnsetVariable)
}

/// A nested expression.
///
/// Expressions may be nested with parentheses or the shorthand $expr.
fn nested<'a>() -> Parser<'a, Expression> {
    (sym(Token::OpenParen) * spacenl() * expression() - spacenl() - sym(Token::CloseParen))
        | (sym(Token::Dollar)
            * not_a(|t: Token| t == Token::Whitespace || t == Token::Newline)
                .repeat(1..)
                .convert(|e| expression().parse(&e)))
}

/// An expression in argument position (words are interpreted as strings rather than as commands).
fn arg<'a>() -> Parser<'a, Expression> {
    call(|| nested() | word().map(Expression::String) | array() | block())
}

/// A command expression.
fn command<'a>() -> Parser<'a, Expression> {
    (arg() + (req_space() * list(arg(), req_space())).opt())
        .map(|(cmd, args)| Expression::Command(Box::new(cmd), args.unwrap_or(vec![])))
}

/// A function expression.
fn function<'a>() -> Parser<'a, Expression> {
    let tok = tag("fn");
    let rest = req_space() * expression().map(|e| Expression::Function(Box::new(e)));
    tok * rest.expect("fn argument")
}

/// An if expression.
fn if_expr<'a>() -> Parser<'a, Expression> {
    let tok = tag("if");
    let rest = (req_space() * arg() - req_space() + arg() - req_space() + arg())
        .map(|((cond, t), f)| Expression::If(Box::new(cond), Box::new(t), Box::new(f)));
    tok * rest.expect("if arguments")
}

/// An array expression.
fn array<'a>() -> Parser<'a, Expression> {
    sym(Token::OpenBracket) * spacenl() * list(arg(), eoe()).map(Expression::Array)
        - spacenl()
        - sym(Token::CloseBracket)
}

/// A block expression.
fn block<'a>() -> Parser<'a, Expression> {
    sym(Token::OpenCurly) * spacenl() * list(expression(), eoe()).map(Expression::Block)
        - spacenl()
        - sym(Token::CloseCurly)
}

/// A single expression.
fn expression<'a>() -> Parser<'a, Expression> {
    call(|| {
        let expr = function()
            | if_expr()
            | set_variable()
            | unset_variable()
            | array()
            | block()
            | command();
        (expr + (sym(Token::Colon) * list(word(), sym(Token::Colon))).opt()).map(|(exp, inds)| {
            let mut e = exp;
            for i in inds.unwrap_or(vec![]) {
                e = Expression::Index(Box::new(e), i);
            }
            e
        })
    })
}

/// Zero or more expressions.
fn exprs<'a>() -> Parser<'a, Vec<Expression>> {
    list(expression(), eoe())
}

/// A single string.
fn word<'a>() -> Parser<'a, String> {
    Parser::new(|a, s| {
        if let Some(t) = a.get(s) {
            if let Token::String(st) = t {
                Ok((st.clone(), s + 1))
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

/// A string token with the given contents.
fn tag<'a>(s: &'a str) -> Parser<'a, ()> {
    let tok = Token::String(s.to_string());
    is_a(move |t: Token| t == tok).discard()
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

    type Result = pom::Result<()>;

    #[test]
    fn word() -> Result {
        assert_parse(
            &[Token::String("hello".to_owned())],
            super::word(),
            "hello".to_owned(),
        )
    }

    #[test]
    fn command() -> Result {
        assert_expr(
            &[
                Token::String("echo".to_owned()),
                Token::Whitespace,
                Token::String("howdy".to_owned()),
            ],
            Expression::Command(
                Box::new(Expression::String("echo".to_owned())),
                vec![Expression::String("howdy".to_owned())],
            ),
        )
    }

    #[test]
    fn arg() -> Result {
        assert_parse(
            &[Token::Dollar, Token::String("howdy".to_owned())],
            super::arg(),
            Expression::Command(Box::new(Expression::String("howdy".to_owned())), vec![]),
        )?;
        assert_parse(
            &[
                Token::OpenParen,
                Token::String("hello".to_owned()),
                Token::Whitespace,
                Token::String("world".to_owned()),
                Token::CloseParen,
            ],
            super::arg(),
            Expression::Command(
                Box::new(Expression::String("hello".to_owned())),
                vec![Expression::String("world".to_owned())],
            ),
        )
    }

    #[test]
    fn array() -> Result {
        assert_expr(
            &[
                Token::OpenBracket,
                Token::Whitespace,
                Token::Dollar,
                Token::String("a".to_owned()),
                Token::Whitespace,
                Token::Comma,
                Token::String("b".to_owned()),
                Token::Whitespace,
                Token::Semicolon,
                Token::Newline,
                Token::String("c".to_owned()),
                Token::Newline,
                Token::Dollar,
                Token::String("d".to_owned()),
                Token::Whitespace,
                Token::CloseBracket,
            ],
            Expression::Array(vec![
                Expression::Command(Box::new(Expression::String("a".to_owned())), vec![]),
                Expression::String("b".to_owned()),
                Expression::String("c".to_owned()),
                Expression::Command(Box::new(Expression::String("d".to_owned())), vec![]),
            ]),
        )
    }

    #[test]
    fn set_variable() -> Result {
        assert_expr(
            &[
                Token::String("a".to_owned()),
                Token::Equal,
                Token::String("echo".to_owned()),
            ],
            Expression::SetVariable("a".to_owned(), Expression::String("echo".to_owned()).into()),
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
                Token::Dollar,
                Token::String("echo".to_owned()),
                Token::Newline,
                Token::CloseCurly,
            ],
            Expression::Block(vec![
                Expression::Command(Box::new(Expression::String("a".to_owned())), vec![]),
                Expression::Command(Box::new(Expression::String("b".to_owned())), vec![]),
                Expression::SetVariable(
                    "c".to_owned(),
                    Box::new(Expression::Command(
                        Box::new(Expression::String("echo".to_owned())),
                        vec![],
                    )),
                ),
            ]),
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
            Expression::Function(Box::new(Expression::Command(
                Box::new(Expression::String("howdy".to_owned())),
                vec![],
            ))),
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
                Box::new(Expression::Command(
                    Box::new(Expression::String("a".to_owned())),
                    vec![],
                )),
                Box::new(Expression::Command(
                    Box::new(Expression::String("b".to_owned())),
                    vec![],
                )),
                Box::new(Expression::Command(
                    Box::new(Expression::String("c".to_owned())),
                    vec![],
                )),
            ),
        )
    }

    #[test]
    fn index() -> Result {
        assert_expr(
            &[
                Token::String("hi".to_owned()),
                Token::Colon,
                Token::String("a".to_owned()),
            ],
            Expression::Index(
                Box::new(Expression::Command(
                    Box::new(Expression::String("hi".to_owned())),
                    vec![],
                )),
                "a".to_owned(),
            ),
        )?;
        assert_expr(
            &[
                Token::String("hi".to_owned()),
                Token::Colon,
                Token::String("a".to_owned()),
                Token::Colon,
                Token::String("b".to_owned()),
            ],
            Expression::Index(
                Expression::Index(
                    Expression::Command(Expression::String("hi".into()).into(), vec![]).into(),
                    "a".into(),
                )
                .into(),
                "b".into(),
            ),
        )
    }

    fn assert_expr(s: &[Token], expected: Expression) -> Result {
        assert_parse(s, expression(), expected)
    }

    fn assert_parse<'a, T: PartialEq + std::fmt::Debug>(
        s: &'a [Token],
        parser: Parser<'a, T>,
        expected: T,
    ) -> Result {
        let r = parser.parse(s)?;
        assert!(r == expected);
        Ok(())
    }
}