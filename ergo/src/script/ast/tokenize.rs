//! AST tokenization.

use ergo_runtime::source::Source;
use std::fmt;

const MAX_TOKEN_LENGTH: u8 = 3;

struct LookaheadIterator<I: Iterator> {
    inner: I,
    lookahead: [Option<I::Item>; MAX_TOKEN_LENGTH as usize],
    next: u8,
}

impl<I: Iterator> LookaheadIterator<I> {
    pub fn new(iter: I) -> Self {
        let mut ret = LookaheadIterator {
            inner: iter,
            lookahead: Default::default(),
            next: 0,
        };
        for i in 0..(MAX_TOKEN_LENGTH as usize) {
            ret.lookahead[i] = ret.inner.next();
        }
        ret
    }

    pub fn peek(&self) -> Option<&I::Item> {
        self.peek_at(0)
    }

    pub fn peek_at(&self, to: usize) -> Option<&I::Item> {
        debug_assert!(to < MAX_TOKEN_LENGTH as usize);
        self.lookahead[(self.next as usize + to) % MAX_TOKEN_LENGTH as usize].as_ref()
    }

    pub fn peek_match(&self, rest: &[I::Item]) -> bool
    where
        I::Item: PartialEq,
    {
        debug_assert!(rest.len() <= MAX_TOKEN_LENGTH as usize);
        for i in 0..rest.len() {
            if !self.peek_at(i).map(|v| v == &rest[i]).unwrap_or(false) {
                return false;
            }
        }
        true
    }
}

impl<I: Iterator> Iterator for LookaheadIterator<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        let ret = std::mem::replace(&mut self.lookahead[self.next as usize], self.inner.next());
        self.next += 1;
        self.next %= MAX_TOKEN_LENGTH;
        ret
    }
}

/// Script tokens.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    /// A string, either from quoted or unquoted words.
    String(String),
    OpenBracket,
    CloseBracket,
    OpenCurly,
    CloseCurly,
    OpenParen,
    CloseParen,
    Comma,
    Semicolon,
    Equal,
    Caret,
    Colon,
    Pipe,
    PipeLeft,
    PipeRight,
    Newline,
    /// One or more whitespace characters (except newlines).
    Whitespace,
}

/// A token with source information.
pub type Tok = Source<Token>;

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::String(s) => write!(f, "{}", s),
            Token::OpenBracket => write!(f, "["),
            Token::CloseBracket => write!(f, "]"),
            Token::OpenCurly => write!(f, "{{"),
            Token::CloseCurly => write!(f, "}}"),
            Token::OpenParen => write!(f, "("),
            Token::CloseParen => write!(f, ")"),
            Token::Comma => write!(f, ","),
            Token::Semicolon => write!(f, ";"),
            Token::Equal => write!(f, "="),
            Token::Caret => write!(f, "^"),
            Token::Colon => write!(f, ":"),
            Token::Pipe => write!(f, "|"),
            Token::PipeLeft => write!(f, "<|"),
            Token::PipeRight => write!(f, "|>"),
            Token::Newline => write!(f, "\n"),
            Token::Whitespace => write!(f, " "),
        }
    }
}

impl PartialEq<Source<Self>> for Token {
    fn eq(&self, other: &Source<Self>) -> bool {
        self == &**other
    }
}

/// Tokenization errors.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error {
    /// A quoted string is missing a closing quote.
    UnfinishedQuotedString,
    /// An escape sequence in a quoted string was not recognized.
    UnrecognizedEscapeSequence,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::UnfinishedQuotedString => write!(f, "unfinished quoted string"),
            Error::UnrecognizedEscapeSequence => write!(f, "unrecognized escape sequence"),
        }
    }
}

impl std::error::Error for Error {}

/// Iterator producing tokens or errors.
pub struct Tokens<I: Iterator> {
    iter: LookaheadIterator<I>,
    source: Source<()>,
}

impl<I: Iterator, T> From<Source<T>> for Tokens<I>
where
    T: IntoIterator<IntoIter = I, Item = I::Item>,
{
    fn from(s: Source<T>) -> Self {
        let (source, i) = s.take();
        Tokens {
            iter: LookaheadIterator::new(i.into_iter()),
            source,
        }
    }
}

impl<I: Iterator<Item = char>> Iterator for Tokens<I> {
    type Item = Result<Tok, Source<Error>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().and_then(|c| {
            self.source.location.start += self.source.location.length;
            self.source.location.length = 1;
            // Comments
            if c == '#' {
                while let Some(c) = self.iter.peek() {
                    if *c != '\n' {
                        self.iter.next();
                        self.source.location.length += 1;
                    } else {
                        break;
                    }
                }
                self.next()
            } else {
                // The remaining parsing will always result in some token based on the consumed
                // character(s).
                let tokentype =
                    // Quoted strings
                    if c == '"' {
                        let mut s = String::new();
                        let mut escape = false;
                        while let Some(c) = self.iter.next() {
                            self.source.location.length += 1;
                            // Escape sequence
                            if escape {
                                if "\"\\".contains(c) {
                                    s.push(c)
                                } else if c == 'n' {
                                    s.push('\n')
                                } else if c == 't' {
                                    s.push('\t')
                                } else {
                                    let mut val = self.source.clone().with(Error::UnrecognizedEscapeSequence);
                                    val.location.start = val.location.start + val.location.length - 2;
                                    val.location.length = 2;
                                    return Some(Err(val));
                                }
                                escape = false;
                            // Normal character
                            } else {
                                if c == '"' {
                                    return Some(Ok(self.source.clone().with(Token::String(s))));
                                } else if c == '\\' {
                                    escape = true;
                                } else {
                                    s.push(c);
                                }
                            }
                        }
                        Err(Error::UnfinishedQuotedString)
                    }
                    // Newlines
                    else if c == '\n' {
                        Ok(Token::Newline)
                    }
                    // General whitespace
                    else if c.is_whitespace() {
                        while let Some(c) = self.iter.peek() {
                            if c.is_whitespace() && *c != '\n' {
                                self.iter.next();
                                self.source.location.length += 1;
                            } else {
                                break;
                            }
                        }
                        Ok(Token::Whitespace)
                    }
                    // Special characters
                    else if c == '[' {
                        Ok(Token::OpenBracket)
                    } else if c == ']' {
                        Ok(Token::CloseBracket)
                    } else if c == '{' {
                        Ok(Token::OpenCurly)
                    } else if c == '}' {
                        Ok(Token::CloseCurly)
                    } else if c == '(' {
                        Ok(Token::OpenParen)
                    } else if c == ')' {
                        Ok(Token::CloseParen)
                    } else if c == ',' {
                        Ok(Token::Comma)
                    } else if c == ';' {
                        Ok(Token::Semicolon)
                    } else if c == '=' {
                        Ok(Token::Equal)
                    } else if c == '^' {
                        Ok(Token::Caret)
                    } else if c == ':' {
                        Ok(Token::Colon)
                    } else if c == '|' {
                        Ok(if self.iter.peek_match(&['>']) {
                            self.iter.next();
                            Token::PipeRight
                        } else { Token::Pipe })
                    } else if c == '<' && self.iter.peek_match(&['|']) {
                        self.iter.next();
                        Ok(Token::PipeLeft)
                    }
                    // Words
                    else {
                        let mut s = String::new();
                        s.push(c);
                        while let Some(c) = self.iter.peek() {
                            if c.is_whitespace() || "[](){},;=^|:".contains(*c) || self.iter.peek_match(&['<','|']) {
                                break;
                            } else {
                                s.push(self.iter.next().unwrap());
                                self.source.location.length += 1;
                            }
                        }
                        Ok(Token::String(s))
                    };
                Some(match tokentype {
                    Ok(t) => Ok(self.source.clone().map(move |()| t)),
                    Err(e) => Err(self.source.clone().map(move |()| e)),
                })
            }
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn symbols() -> Result<(), Source<Error>> {
        assert_tokens(
            "[]{}(),;=^:|<||>\n",
            &[
                Token::OpenBracket,
                Token::CloseBracket,
                Token::OpenCurly,
                Token::CloseCurly,
                Token::OpenParen,
                Token::CloseParen,
                Token::Comma,
                Token::Semicolon,
                Token::Equal,
                Token::Caret,
                Token::Colon,
                Token::Pipe,
                Token::PipeLeft,
                Token::PipeRight,
                Token::Newline,
            ],
        )
    }

    #[test]
    fn whitespace() -> Result<(), Source<Error>> {
        assert_tokens(
            " ,     \n   ",
            &[
                Token::Whitespace,
                Token::Comma,
                Token::Whitespace,
                Token::Newline,
                Token::Whitespace,
            ],
        )
    }

    #[test]
    fn strings() -> Result<(), Source<Error>> {
        assert_tokens(
            "hello \"world[]{}():,;=^|\" \"escape\\\"quote\\n\"",
            &[
                Token::String("hello".to_owned()),
                Token::Whitespace,
                Token::String("world[]{}():,;=^|".to_owned()),
                Token::Whitespace,
                Token::String("escape\"quote\n".to_owned()),
            ],
        )
    }

    #[test]
    fn string_ends() -> Result<(), Source<Error>> {
        assert_tokens(
            "a[b]c{d}e(f)g:h,i;j^k=l\nm n|o<|p|>",
            &[
                Token::String("a".to_owned()),
                Token::OpenBracket,
                Token::String("b".to_owned()),
                Token::CloseBracket,
                Token::String("c".to_owned()),
                Token::OpenCurly,
                Token::String("d".to_owned()),
                Token::CloseCurly,
                Token::String("e".to_owned()),
                Token::OpenParen,
                Token::String("f".to_owned()),
                Token::CloseParen,
                Token::String("g".to_owned()),
                Token::Colon,
                Token::String("h".to_owned()),
                Token::Comma,
                Token::String("i".to_owned()),
                Token::Semicolon,
                Token::String("j".to_owned()),
                Token::Caret,
                Token::String("k".to_owned()),
                Token::Equal,
                Token::String("l".to_owned()),
                Token::Newline,
                Token::String("m".to_owned()),
                Token::Whitespace,
                Token::String("n".to_owned()),
                Token::Pipe,
                Token::String("o".to_owned()),
                Token::PipeLeft,
                Token::String("p".to_owned()),
                Token::PipeRight,
            ],
        )
    }

    #[test]
    fn bad_escape() {
        let err = assert_tokens("\"ohn\\o\"", &[]).unwrap_err().unwrap();
        assert!(err == Error::UnrecognizedEscapeSequence);
    }

    #[test]
    fn unfinished_string() {
        let err = assert_tokens("\"ohno", &[]).unwrap_err().unwrap();
        assert!(err == Error::UnfinishedQuotedString);
    }

    #[test]
    fn comments() -> Result<(), Source<Error>> {
        assert_tokens(
            "# This is a comment\n: #This is also a comment\n#One last comment",
            &[
                Token::Newline,
                Token::Colon,
                Token::Whitespace,
                Token::Newline,
            ],
        )
    }

    fn assert_tokens(s: &str, expected: &[Token]) -> Result<(), Source<Error>> {
        let toks: Vec<_> = Tokens::from(
            Source::new(super::super::StringSource::new("<string>", s.to_owned()))
                .open()
                .unwrap(),
        )
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .map(|t| t.unwrap())
        .collect();
        assert!(toks == expected);
        Ok(())
    }
}
