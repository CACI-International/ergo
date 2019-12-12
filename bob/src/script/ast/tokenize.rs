//! AST tokenization.

use super::Location;
use std::fmt;

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
    Dollar,
    Comma,
    Semicolon,
    Colon,
    Equal,
    Newline,
    /// One or more whitespace characters (except newlines).
    Whitespace,
}

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
            Token::Dollar => write!(f, "$"),
            Token::Comma => write!(f, ","),
            Token::Semicolon => write!(f, ";"),
            Token::Colon => write!(f, ":"),
            Token::Equal => write!(f, "="),
            Token::Newline => write!(f, "\n"),
            Token::Whitespace => write!(f, " "),
        }
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

/// Iterator producing tokens or errors.
pub struct Tokens<I: Iterator> {
    iter: std::iter::Peekable<I>,
    count: usize,
}

impl<I: Iterator> Tokens<I> {
    pub fn new<T: IntoIterator<IntoIter = I, Item = I::Item>>(i: T) -> Self {
        Tokens {
            iter: i.into_iter().peekable(),
            count: 0,
        }
    }
}

impl<I: Iterator<Item = char>> Iterator for Tokens<I> {
    type Item = Result<(Token, Location), (Error, Location)>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().and_then(|c| {
            let start = self.count;
            self.count += 1;
            // Comments
            if c == '#' {
                while let Some(c) = self.iter.peek() {
                    if *c != '\n' {
                        self.iter.next();
                        self.count += 1;
                    } else {
                        break;
                    }
                }
                self.next()
            } else {
                // The remaining parsing will always result in some token based on the consumed
                // character.
                let tokentype =
                    // Quoted strings
                    if c == '"' {
                        let mut s = String::new();
                        let mut escape = false;
                        while let Some(c) = self.iter.next() {
                            self.count += 1;
                            // Escape sequence
                            if escape {
                                if "\"\\".contains(c) {
                                    s.push(c)
                                } else if c == 'n' {
                                    s.push('\n');
                                } else if c == 't' {
                                    s.push('\t');
                                } else {
                                    return Some(Err((Error::UnrecognizedEscapeSequence,Location::new(self.count-1, 2))));
                                }
                                escape = false;
                            // Normal character
                            } else {
                                if c == '"' {
                                    return Some(Ok((Token::String(s),Location::new(start,self.count - start))));
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
                                self.count += 1;
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
                    } else if c == '$' {
                        Ok(Token::Dollar)
                    } else if c == ',' {
                        Ok(Token::Comma)
                    } else if c == ';' {
                        Ok(Token::Semicolon)
                    } else if c == ':' {
                        Ok(Token::Colon)
                    } else if c == '=' {
                        Ok(Token::Equal)
                    }
                    // Words
                    else {
                        let mut s = String::new();
                        s.push(c);
                        while let Some(c) = self.iter.peek() {
                            if c.is_whitespace() || "[](){},;:$=".contains(*c) {
                                break;
                            } else {
                                s.push(self.iter.next().unwrap());
                                self.count += 1;
                            }
                        }
                        Ok(Token::String(s))
                    };
                let loc = Location::new(start, self.count - start);
                Some(match tokentype {
                    Ok(t) => Ok((t,loc)),
                    Err(e) => Err((e,loc)),
                })
            }
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn symbols() -> Result<(), (Error, Location)> {
        assert_tokens(
            "[]{}()$,;:=\n",
            &[
                Token::OpenBracket,
                Token::CloseBracket,
                Token::OpenCurly,
                Token::CloseCurly,
                Token::OpenParen,
                Token::CloseParen,
                Token::Dollar,
                Token::Comma,
                Token::Semicolon,
                Token::Colon,
                Token::Equal,
                Token::Newline,
            ],
        )
    }

    #[test]
    fn whitespace() -> Result<(), (Error, Location)> {
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
    fn strings() -> Result<(), (Error, Location)> {
        assert_tokens(
            "hello \"world[]{}()$:,;:=\" \"escape\\\"quote\\n\"",
            &[
                Token::String("hello".to_owned()),
                Token::Whitespace,
                Token::String("world[]{}()$:,;:=".to_owned()),
                Token::Whitespace,
                Token::String("escape\"quote\n".to_owned()),
            ],
        )
    }

    #[test]
    fn string_ends() -> Result<(), (Error, Location)> {
        assert_tokens(
            "a[b]c{d}e(f)g$h,i;j:k=l\nm ",
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
                Token::Dollar,
                Token::String("h".to_owned()),
                Token::Comma,
                Token::String("i".to_owned()),
                Token::Semicolon,
                Token::String("j".to_owned()),
                Token::Colon,
                Token::String("k".to_owned()),
                Token::Equal,
                Token::String("l".to_owned()),
                Token::Newline,
                Token::String("m".to_owned()),
                Token::Whitespace,
            ],
        )
    }

    #[test]
    fn bad_escape() {
        let (err, _) = assert_tokens("\"ohn\\o\"", &[]).unwrap_err();
        assert!(err == Error::UnrecognizedEscapeSequence);
    }

    #[test]
    fn unfinished_string() {
        let (err, _) = assert_tokens("\"ohno", &[]).unwrap_err();
        assert!(err == Error::UnfinishedQuotedString);
    }

    #[test]
    fn comments() -> Result<(), (Error, Location)> {
        assert_tokens(
            "# This is a comment\n$ #This is also a comment\n#One last comment",
            &[
                Token::Newline,
                Token::Dollar,
                Token::Whitespace,
                Token::Newline,
            ],
        )
    }

    fn assert_tokens(s: &str, expected: &[Token]) -> Result<(), (Error, Location)> {
        let toks: Vec<_> = Tokens::new(s.chars())
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .map(|(t, _)| t)
            .collect();
        assert!(toks == expected);
        Ok(())
    }
}
