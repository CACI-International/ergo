//! AST parsing.

use super::*;
use super::tokenize::Token;
use pom::parser::*;

/// A parser alias.
pub type Parser<'a, T> = pom::parser::Parser<'a, Token, T>;

/// Return a parser for a script.
pub fn script<'a>() -> Parser<'a, Script> {
    exprs()
}

/// A set variable expression.
fn set_variable<'a>() -> Parser<'a, Expression> {
    let parsed = word() - space() - sym(Token::Equal).discard() - space() + expression();
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
    sym(Token::OpenParen) * spacenl() * expression() - spacenl() - sym(Token::CloseParen)
        | sym(Token::Dollar)
            * not_a(|t: Token| t == Token::Whitespace || t == Token::Newline)
                .repeat(1..)
                .convert(|e| expression().parse(&e))
}

/// An expression in argument position (words are interpreted as strings rather than as commands).
fn arg<'a>() -> Parser<'a, Expression> {
    nested() | word().map(Expression::String) | expression()
}

/// A command expression.
fn command<'a>() -> Parser<'a, Expression> {
    (arg() - space() + list(arg(), req_space())).map(|(cmd, args)| Expression::Command(Box::new(cmd), args))
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
    let rest = (req_space() * expression() - req_space() + expression() - req_space()
        + expression())
    .map(|((cond, t), f)| Expression::If(Box::new(cond), Box::new(t), Box::new(f)));
    tok * rest.expect("if arguments")
}

/// An array expression.
fn array<'a>() -> Parser<'a, Expression> {
    sym(Token::OpenBracket) * spacenl() * list(arg(), eoe()).map(Expression::Array) - spacenl() - sym(Token::CloseBracket)
}

/// A block expression.
fn block<'a>() -> Parser<'a, Expression> {
    sym(Token::OpenCurly) * spacenl() * list(expression(), eoe()).map(Expression::Block) - spacenl() - sym(Token::CloseCurly)
}

/// An index expression.
fn index<'a>() -> Parser<'a, Expression> {
    (expression() - sym(Token::Colon) + word()).map(|(exp, name)| Expression::Index(Box::new(exp), name))
}

/// A single expression.
fn expression<'a>() -> Parser<'a, Expression> {
    call(|| {
            function()
            | if_expr()
            | set_variable()
            | unset_variable()
            | array()
            | block()
            | index()
            | command()
    })
}

/// Zero or more expressions.
fn exprs<'a>() -> Parser<'a, Vec<Expression>> {
    list(expression(), eoe())
}

/// A single string.
fn word<'a>() -> Parser<'a, String> {
    Parser::new(|a,s| {
        if let Some(t) = a.get(s) {
            if let Token::String(st) = t {
                Ok((st.clone(),s+1))
            } else {
                Err(pom::Error::Mismatch {
                    message: format!("expected string, found {}", t),
                    position: s
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
    one_of([Token::Newline,Token::Whitespace].as_ref()).repeat(0..).discard()
}

/// End of expression delimiter.
fn eoe<'a>() -> Parser<'a, ()> {
    space() * one_of([Token::Newline,Token::Comma,Token::Semicolon].as_ref()) * spacenl()
}
