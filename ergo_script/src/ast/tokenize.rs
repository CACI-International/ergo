//! AST tokenization.

use ergo_runtime::source::Source;
use std::fmt;

const MAX_TOKEN_LOOKAHEAD: u8 = 2;

struct LookaheadIterator<I: Iterator> {
    inner: I,
    lookahead: [Option<I::Item>; MAX_TOKEN_LOOKAHEAD as usize],
    next: u8,
}

impl<I: Iterator> LookaheadIterator<I> {
    pub fn new(iter: I) -> Self {
        let mut ret = LookaheadIterator {
            inner: iter,
            lookahead: Default::default(),
            next: 0,
        };
        for i in 0..(MAX_TOKEN_LOOKAHEAD as usize) {
            ret.lookahead[i] = ret.inner.next();
        }
        ret
    }

    pub fn peek(&self) -> Option<&I::Item> {
        self.peek_at(0)
    }

    pub fn peek_at(&self, to: usize) -> Option<&I::Item> {
        debug_assert!(to < MAX_TOKEN_LOOKAHEAD as usize);
        self.lookahead[(self.next as usize + to) % MAX_TOKEN_LOOKAHEAD as usize].as_ref()
    }

    pub fn peek_match(&self, rest: &[I::Item]) -> bool
    where
        I::Item: PartialEq,
    {
        debug_assert!(rest.len() <= MAX_TOKEN_LOOKAHEAD as usize);
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
        self.next %= MAX_TOKEN_LOOKAHEAD;
        ret
    }
}

/// Tokens relevant to expression interpretation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SymbolicToken {
    /// A string, either from quoted or unquoted words.
    String(String),
    /// A doc comment block.
    DocComment(String),
    Equal,
    Caret,
    Colon,
    Bang,
    Arrow,
    Pipe,
    PipeLeft,
    PipeRight,
}

/// Tokens which are parsed in pairs.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PairedToken {
    Paren,
    Curly,
    Bracket,
}

/// Script tokens.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Symbol(SymbolicToken),
    Pair {
        which: PairedToken,
        open: bool,
    },
    Comma,
    Semicolon,
    Newline,
    /// One or more whitespace characters (except newlines).
    Whitespace,
}

impl Token {
    pub fn string<S: Into<String>>(s: S) -> Self {
        Token::Symbol(SymbolicToken::String(s.into()))
    }

    pub fn doc_comment<S: Into<String>>(s: S) -> Self {
        Token::Symbol(SymbolicToken::DocComment(s.into()))
    }

    pub fn equal() -> Self {
        Token::Symbol(SymbolicToken::Equal)
    }

    pub fn caret() -> Self {
        Token::Symbol(SymbolicToken::Caret)
    }

    pub fn colon() -> Self {
        Token::Symbol(SymbolicToken::Colon)
    }

    pub fn bang() -> Self {
        Token::Symbol(SymbolicToken::Bang)
    }

    pub fn arrow() -> Self {
        Token::Symbol(SymbolicToken::Arrow)
    }

    pub fn open_paren() -> Self {
        Token::Pair {
            which: PairedToken::Paren,
            open: true,
        }
    }

    pub fn close_paren() -> Self {
        Token::Pair {
            which: PairedToken::Paren,
            open: false,
        }
    }

    pub fn open_curly() -> Self {
        Token::Pair {
            which: PairedToken::Curly,
            open: true,
        }
    }

    pub fn close_curly() -> Self {
        Token::Pair {
            which: PairedToken::Curly,
            open: false,
        }
    }

    pub fn open_bracket() -> Self {
        Token::Pair {
            which: PairedToken::Bracket,
            open: true,
        }
    }

    pub fn close_bracket() -> Self {
        Token::Pair {
            which: PairedToken::Bracket,
            open: false,
        }
    }

    pub fn pipe() -> Self {
        Token::Symbol(SymbolicToken::Pipe)
    }

    pub fn pipe_left() -> Self {
        Token::Symbol(SymbolicToken::PipeLeft)
    }

    pub fn pipe_right() -> Self {
        Token::Symbol(SymbolicToken::PipeRight)
    }

    pub fn implies_value_when_before(&self) -> bool {
        match self {
            Token::Symbol(SymbolicToken::PipeRight)
            | Token::Symbol(SymbolicToken::String(_))
            | Token::Pair { open: false, .. } => true,
            _ => false,
        }
    }

    pub fn implies_value_when_after(&self) -> bool {
        match self {
            Token::Symbol(SymbolicToken::PipeLeft)
            | Token::Symbol(SymbolicToken::String(_))
            | Token::Symbol(SymbolicToken::Colon)
            | Token::Symbol(SymbolicToken::Bang)
            | Token::Pair { open: true, .. } => true,
            _ => false,
        }
    }
}

impl fmt::Display for SymbolicToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use SymbolicToken::*;
        match self {
            String(s) => write!(f, "{}", s),
            DocComment(s) => {
                for line in s.split("\n") {
                    writeln!(f, "## {}", line)?;
                }
                Ok(())
            }
            Equal => write!(f, "="),
            Caret => write!(f, "^"),
            Colon => write!(f, ":"),
            Bang => write!(f, "!"),
            Arrow => write!(f, "->"),
            Pipe => write!(f, "|"),
            PipeLeft => write!(f, "<|"),
            PipeRight => write!(f, "|>"),
        }
    }
}

impl fmt::Display for PairedToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PairedToken::Paren => write!(f, "parenthesis"),
            PairedToken::Bracket => write!(f, "square bracket"),
            PairedToken::Curly => write!(f, "curly bracket"),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Symbol(t) => write!(f, "{}", t),
            Token::Pair {
                which: PairedToken::Paren,
                open,
            } => {
                if *open {
                    write!(f, "(")
                } else {
                    write!(f, ")")
                }
            }
            Token::Pair {
                which: PairedToken::Bracket,
                open,
            } => {
                if *open {
                    write!(f, "[")
                } else {
                    write!(f, "]")
                }
            }
            Token::Pair {
                which: PairedToken::Curly,
                open,
            } => {
                if *open {
                    write!(f, "{{")
                } else {
                    write!(f, "}}")
                }
            }
            Token::Comma => write!(f, ","),
            Token::Semicolon => write!(f, ";"),
            Token::Newline => write!(f, "\n"),
            Token::Whitespace => write!(f, " "),
        }
    }
}

impl PartialEq<Source<Self>> for PairedToken {
    fn eq(&self, other: &Source<Self>) -> bool {
        self == &**other
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
    type Item = Result<Source<Token>, Source<Error>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().and_then(|c| {
            self.source.location.start += self.source.location.length;
            self.source.location.length = 1;
            // Comments
            if c == '#' {
                // Doc comment
                if self.iter.peek_match(&['#',' ']) {
                    self.iter.next();
                    self.iter.next();
                    self.source.location.length += 2;

                    let mut s = String::new();
                    while let Some(c) = self.iter.peek() {
                        if *c != '\n' {
                            s.push(*c);
                            self.iter.next();
                            self.source.location.length += 1;
                        } else {
                            break;
                        }
                    }
                    Some(Ok(self.source.clone().with(Token::doc_comment(s))))
                } else {
                    while let Some(c) = self.iter.peek() {
                        if *c != '\n' {
                            self.iter.next();
                            self.source.location.length += 1;
                        } else {
                            break;
                        }
                    }
                    self.next()
                }
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
                                    return Some(Ok(self.source.clone().with(Token::string(s))));
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
                        Ok(Token::open_bracket())
                    } else if c == ']' {
                        Ok(Token::close_bracket())
                    } else if c == '{' {
                        Ok(Token::open_curly())
                    } else if c == '}' {
                        Ok(Token::close_curly())
                    } else if c == '(' {
                        Ok(Token::open_paren())
                    } else if c == ')' {
                        Ok(Token::close_paren())
                    } else if c == ',' {
                        Ok(Token::Comma)
                    } else if c == ';' {
                        Ok(Token::Semicolon)
                    } else if c == '=' {
                        Ok(Token::equal())
                    } else if c == '^' {
                        Ok(Token::caret())
                    } else if c == ':' {
                        Ok(Token::colon())
                    } else if c == '!' {
                        Ok(Token::bang())
                    } else if c == '-' && self.iter.peek_match(&['>']) {
                        self.iter.next();
                        self.source.location.length += 1;
                        Ok(Token::arrow())
                    } else if c == '|' {
                        Ok(if self.iter.peek_match(&['>']) {
                            self.iter.next();
                            self.source.location.length += 1;
                            Token::pipe_right()
                        } else { Token::pipe() })
                    } else if c == '<' && self.iter.peek_match(&['|']) {
                        self.iter.next();
                        self.source.location.length += 1;
                        Ok(Token::pipe_left())
                    } else {
                        // Words
                        let mut s = String::new();
                        s.push(c);
                        while let Some(c) = self.iter.peek() {
                            if c.is_whitespace() || "[](){},;=^|:!".contains(*c) || self.iter.peek_match(&['<','|'])
                                || self.iter.peek_match(&['-','>']) {
                                break;
                            } else {
                                s.push(self.iter.next().unwrap());
                                self.source.location.length += 1;
                            }
                        }
                        Ok(Token::string(s))
                    };
                Some(match tokentype {
                    Ok(t) => Ok(self.source.clone().with(t)),
                    Err(e) => Err(self.source.clone().with(e)),
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
            "[]{}(),;=^:!->|<||>\n",
            &[
                Token::open_bracket(),
                Token::close_bracket(),
                Token::open_curly(),
                Token::close_curly(),
                Token::open_paren(),
                Token::close_paren(),
                Token::Comma,
                Token::Semicolon,
                Token::equal(),
                Token::caret(),
                Token::colon(),
                Token::bang(),
                Token::arrow(),
                Token::pipe(),
                Token::pipe_left(),
                Token::pipe_right(),
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
            "hello \"world[]{}():!,;=^|<||>->\" \"escape\\\"quote\\n\"",
            &[
                Token::string("hello"),
                Token::Whitespace,
                Token::string("world[]{}():!,;=^|<||>->"),
                Token::Whitespace,
                Token::string("escape\"quote\n"),
            ],
        )
    }

    #[test]
    fn string_ends() -> Result<(), Source<Error>> {
        assert_tokens(
            "a[b]c{d}e(f)g:h,i;j^k=l\nm n|o<|p|>q!r->",
            &[
                Token::string("a"),
                Token::open_bracket(),
                Token::string("b"),
                Token::close_bracket(),
                Token::string("c"),
                Token::open_curly(),
                Token::string("d"),
                Token::close_curly(),
                Token::string("e"),
                Token::open_paren(),
                Token::string("f"),
                Token::close_paren(),
                Token::string("g"),
                Token::colon(),
                Token::string("h"),
                Token::Comma,
                Token::string("i"),
                Token::Semicolon,
                Token::string("j"),
                Token::caret(),
                Token::string("k"),
                Token::equal(),
                Token::string("l"),
                Token::Newline,
                Token::string("m"),
                Token::Whitespace,
                Token::string("n"),
                Token::pipe(),
                Token::string("o"),
                Token::pipe_left(),
                Token::string("p"),
                Token::pipe_right(),
                Token::string("q"),
                Token::bang(),
                Token::string("r"),
                Token::arrow(),
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
                Token::colon(),
                Token::Whitespace,
                Token::Newline,
            ],
        )
    }

    #[test]
    fn doc_comments() -> Result<(), Source<Error>> {
        assert_tokens(
            "## This is a doc comment\nhello world",
            &[
                Token::doc_comment("This is a doc comment"),
                Token::Newline,
                Token::string("hello"),
                Token::Whitespace,
                Token::string("world"),
            ],
        )?;
        assert_tokens(
            "## This is a doc comment\n## more doc comment\n  ##  and more\nhello",
            &[
                Token::doc_comment("This is a doc comment"),
                Token::Newline,
                Token::doc_comment("more doc comment"),
                Token::Newline,
                Token::Whitespace,
                Token::doc_comment(" and more"),
                Token::Newline,
                Token::string("hello"),
            ],
        )?;
        Ok(())
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
        dbg!(&toks);
        assert!(toks == expected);
        Ok(())
    }
}
