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
    String {
        string: String,
        quoted: bool,
    },
    /// A doc string.
    ///
    /// Doc strings will always be present immediately after `Token::DocComment`, and immediately
    /// before `Token::Newline`, even if empty.
    DocString(String),
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
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PairedToken {
    Paren,
    Curly,
    Bracket,
    DocCurly,
}

/// Script tokens.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Symbol(SymbolicToken),
    Pair {
        which: PairedToken,
        open: bool,
    },
    DocComment,
    TreeComment,
    Comma,
    Semicolon,
    Newline,
    /// One or more whitespace characters (except newlines).
    Whitespace,
}

impl Token {
    pub fn string<S: Into<String>>(s: S, quoted: bool) -> Self {
        Token::Symbol(SymbolicToken::String {
            string: s.into(),
            quoted,
        })
    }

    pub fn doc_string<S: Into<String>>(s: S) -> Self {
        Token::Symbol(SymbolicToken::DocString(s.into()))
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

    pub fn open_doc_curly() -> Self {
        Token::Pair {
            which: PairedToken::DocCurly,
            open: true,
        }
    }

    pub fn close_doc_curly() -> Self {
        Token::Pair {
            which: PairedToken::DocCurly,
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
            | Token::Symbol(SymbolicToken::String { .. })
            | Token::Pair { open: false, .. } => true,
            _ => false,
        }
    }

    pub fn implies_value_when_after(&self) -> bool {
        match self {
            Token::Symbol(SymbolicToken::PipeLeft)
            | Token::Symbol(SymbolicToken::String { .. })
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
            // TODO handle quoted
            String { string, .. } => write!(f, "{}", string),
            DocString(s) => write!(f, "{}", s),
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
            PairedToken::DocCurly => write!(f, "doc curly bracket"),
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
            Token::Pair {
                which: PairedToken::DocCurly,
                open,
            } => {
                if *open {
                    write!(f, "{{{{")
                } else {
                    write!(f, "}}}}")
                }
            }
            Token::DocComment => write!(f, "##"),
            Token::TreeComment => write!(f, "#"),
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
    /// A raw string is missing a closing sequence.
    UnfinishedRawString,
    /// An escape sequence in a quoted string was not recognized.
    UnrecognizedEscapeSequence,
    /// A unicode character escape sequence is missing a closing character.
    UnfinishedUnicodeCharacter,
    /// A unicode character escape sequence was too long.
    LongUnicodeCharacter,
    /// A unicode character escape sequence contained an invalid digit.
    InvalidUnicodeCharacter,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::UnfinishedQuotedString => write!(f, "unfinished quoted string"),
            Error::UnfinishedRawString => write!(f, "unfinished raw string"),
            Error::UnrecognizedEscapeSequence => write!(f, "unrecognized escape sequence"),
            Error::UnfinishedUnicodeCharacter => write!(f, "unfinished unicode character code"),
            Error::LongUnicodeCharacter => write!(f, "unicode character code too long"),
            Error::InvalidUnicodeCharacter => write!(f, "invalid unicode character code"),
        }
    }
}

impl std::error::Error for Error {}

impl super::ToDiagnostic for Error {
    fn additional_info(&self, _diagnostic: &mut ergo_runtime::error::Diagnostic) {}
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum DocCommentMode {
    String,
    Expression,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
struct DocComment {
    active: bool,
    mode: DocCommentMode,
}

impl Default for DocComment {
    fn default() -> Self {
        DocComment {
            active: false,
            mode: DocCommentMode::String,
        }
    }
}

impl DocComment {
    fn string() -> Self {
        DocComment {
            active: true,
            mode: DocCommentMode::String,
        }
    }

    fn expr() -> Self {
        DocComment {
            active: true,
            mode: DocCommentMode::Expression,
        }
    }
}

#[derive(Debug)]
enum Pending {
    None,
    String(String, bool, ergo_runtime::source::Location),
    Result(Result<Source<Token>, Source<Error>>),
}

impl Default for Pending {
    fn default() -> Self {
        Pending::None
    }
}

/// Iterator producing tokens or errors.
pub struct Tokens<I: Iterator> {
    iter: LookaheadIterator<I>,
    source: Source<()>,
    doc_comment: DocComment,
    pending: Pending,
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
            doc_comment: DocComment::default(),
            pending: Pending::None,
        }
    }
}

impl<I: Iterator> Tokens<I> {
    fn next_source(&mut self) -> Option<I::Item> {
        let ret = self.iter.next();
        if ret.is_some() {
            self.source.location.length += 1;
        }
        ret
    }

    fn reset_source_start(&mut self) {
        self.source.location.start += self.source.location.length;
        self.source.location.length = 0;
    }
}

impl<I: Iterator<Item = char>> Tokens<I> {
    fn push_str(&mut self, c: char) {
        match &mut self.pending {
            Pending::None => {
                self.pending = Pending::String(c.into(), false, self.source.location.clone());
            }
            Pending::String(s, _, l) => {
                s.push(c);
                *l = &*l + &self.source.location;
            }
            Pending::Result(_) => panic!("cannot push string"),
        }
    }

    fn extend_str(&mut self, s: String) {
        match &mut self.pending {
            Pending::None => {
                self.pending = Pending::String(s, false, self.source.location.clone());
            }
            Pending::String(st, _, l) => {
                st.push_str(&s);
                *l = &*l + &self.source.location;
            }
            Pending::Result(_) => panic!("cannot extend string"),
        }
    }

    fn quoted_str(&mut self) {
        match &mut self.pending {
            Pending::String(_, quoted, _) => {
                *quoted = true;
            }
            _ => panic!("invalid pending string state"),
        }
    }

    fn next_non_string(&mut self) -> Option<Result<Source<Token>, Source<Error>>> {
        self.reset_source_start();

        // If in doc comment string mode, _always_ consume everything until newline/doc curly (even
        // if empty) and emit a doc string. In any case, the doc comment state must be transitioned
        // to something other than `DocComment::string()`.
        if self.doc_comment == DocComment::string() {
            let mut s = String::new();
            while let Some(c) = self.iter.peek() {
                if *c == '\n' {
                    break;
                } else if *c == '{' && self.iter.peek_at(1) == Some(&'{') {
                    self.doc_comment = DocComment::expr();
                    break;
                } else {
                    s.push(self.next_source().unwrap());
                }
            }
            // Newline sets this too, but we need to ensure the doc comment state changes even if
            // self.iter.peek() returned None.
            if self.doc_comment == DocComment::string() {
                self.doc_comment.active = false;
            }
            debug_assert!(self.doc_comment != DocComment::string());
            return Some(Ok(self.source.clone().with(Token::doc_string(s))));
        }

        self.next_source().and_then(|c| {
            if c == '#' {
                let next = self.iter.peek();
                // Doc comment (only when we are not in a doc comment expression)
                if next == Some(&'#') && !self.doc_comment.active {
                    self.next_source();

                    self.doc_comment.active = true;
                    Some(Ok(self.source.clone().with(Token::DocComment)))
                } else if next.map(|c| c.is_whitespace()).unwrap_or(true) || (next == Some(&'#') && self.doc_comment.active) {
                    // Line comment
                    while let Some(c) = self.iter.peek() {
                        if *c != '\n' {
                            self.next_source();
                        } else {
                            break;
                        }
                    }
                    self.next()
                } else {
                    // Tree comment
                    Some(Ok(self.source.clone().with(Token::TreeComment)))
                }
            } else {
                // The remaining parsing will always result in some token based on the consumed
                // character(s).
                let tokentype =
                    // Newlines
                    if c == '\n' {
                        self.doc_comment.active = false;
                        Ok(Token::Newline)
                    }
                    // General whitespace
                    else if c.is_whitespace() {
                        while let Some(c) = self.iter.peek() {
                            if c.is_whitespace() && *c != '\n' {
                                self.next_source();
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
                        // Check for doc open curly if applicable.
                        Ok(if self.doc_comment == DocComment::expr() && self.iter.peek_match(&['{']) {
                            self.next_source();
                            Token::open_doc_curly()
                        } else {
                            Token::open_curly()
                        })
                    } else if c == '}' {
                        // Check for doc closing curly if applicable.
                        Ok(if self.doc_comment == DocComment::expr() && self.iter.peek_match(&['}']) {
                            self.next_source();
                            self.doc_comment = DocComment::string();
                            Token::close_doc_curly()
                        } else {
                            Token::close_curly()
                        })
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
                        self.next_source();
                        Ok(Token::arrow())
                    } else if c == '|' {
                        Ok(if self.iter.peek_match(&['>']) {
                            self.next_source();
                            Token::pipe_right()
                        } else { Token::pipe() })
                    } else if c == '<' && self.iter.peek_match(&['|']) {
                        self.next_source();
                        Ok(Token::pipe_left())
                    }
                    // Quoted strings
                    else if c == '"' {
                        let mut s = String::default();
                        let mut escape = false;
                        while let Some(c) = self.next_source() {
                            // Escape sequence
                            if escape {
                                if "\"\\".contains(c) {
                                    s.push(c)
                                } else if c == 'n' {
                                    s.push('\n')
                                } else if c == 't' {
                                    s.push('\t')
                                // Unicode character codes
                                } else if c == 'u' && self.iter.peek_match(&['{']) {
                                    self.next_source();
                                    let mut unicode_hex = String::new();
                                    loop {
                                        match self.next_source() {
                                            None | Some('"') => {
                                                let mut val = self.source.clone().with(Error::UnfinishedUnicodeCharacter);
                                                val.location.start = val.location.start + val.location.length - unicode_hex.len();
                                                val.location.length = unicode_hex.len();
                                                return Some(Err(val));
                                            }
                                            Some('}') => {
                                                if unicode_hex.is_empty() {
                                                    let mut val = self.source.clone().with(Error::InvalidUnicodeCharacter);
                                                    val.location.start = val.location.start + val.location.length - 2;
                                                    val.location.length = 2;
                                                    return Some(Err(val));
                                                }

                                                match <char as std::convert::TryFrom<u32>>::try_from(u32::from_str_radix(unicode_hex.as_str(), 16)
                                                        .expect("all digits should be valid hex digits"))
                                                {
                                                    Err(_) => {
                                                        let mut val = self.source.clone().with(Error::InvalidUnicodeCharacter);
                                                        val.location.start = val.location.start + val.location.length - unicode_hex.len() - 1;
                                                        val.location.length = unicode_hex.len();
                                                        return Some(Err(val));
                                                    }
                                                    Ok(c) => s.push(c)
                                                }
                                                break;
                                            }
                                            Some(c) => {
                                                if !c.is_ascii_hexdigit() {
                                                    let mut val = self.source.clone().with(Error::InvalidUnicodeCharacter);
                                                    val.location.start = val.location.start + val.location.length - 1;
                                                    val.location.length = 1;
                                                    return Some(Err(val));
                                                }
                                                if unicode_hex.len() == 6 {
                                                    let mut val = self.source.clone().with(Error::LongUnicodeCharacter);
                                                    val.location.start = val.location.start + val.location.length - unicode_hex.len() - 1;
                                                    val.location.length = unicode_hex.len() + 1;
                                                    return Some(Err(val));
                                                }
                                                unicode_hex.push(c);
                                            }
                                        }
                                    }
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
                                    self.extend_str(s);
                                    self.quoted_str();
                                    return self.next_non_string();
                                } else if c == '\\' {
                                    escape = true;
                                } else {
                                    s.push(c);
                                }
                            }
                        }
                        Err(Error::UnfinishedQuotedString)
                    }
                    // Raw strings
                    else if c == '\'' {
                        let mut s = String::default();
                        let mut count = 1;
                        while let Some('\'') = self.iter.peek() {
                            count += 1;
                            self.next_source();
                        }
                        let close_count = count;
                        while let Some(c) = self.next_source() {
                            s.push(c);
                            if c == '\'' {
                                count -= 1;
                                if count == 0 {
                                    break
                                }
                            } else {
                                count = close_count;
                            }
                        }
                        if count != 0 {
                            Err(Error::UnfinishedRawString)
                        } else {
                            s.truncate(s.len() - close_count);
                            self.extend_str(s);
                            self.quoted_str();
                            return self.next_non_string();
                        }
                    } else {
                        self.push_str(c);
                        return self.next_non_string();
                    };
                Some(match tokentype {
                    Ok(t) => Ok(self.source.clone().with(t)),
                    Err(e) => Err(self.source.clone().with(e)),
                })
            }
        })
    }
}

impl<I: Iterator<Item = char>> Iterator for Tokens<I> {
    type Item = Result<Source<Token>, Source<Error>>;

    fn next(&mut self) -> Option<Self::Item> {
        match std::mem::take(&mut self.pending) {
            Pending::String(s, quoted, l) => {
                let mut src = self.source.clone();
                src.location = l;
                return Some(Ok(src.with(Token::string(s, quoted))));
            }
            Pending::Result(r) => {
                return Some(r);
            }
            _ => (),
        }

        let next = self.next_non_string();

        if let Pending::String(s, quoted, l) = std::mem::take(&mut self.pending) {
            self.pending = next.map(Pending::Result).unwrap_or(Pending::None);
            let mut src = self.source.clone();
            src.location = l;
            Some(Ok(src.with(Token::string(s, quoted))))
        } else {
            next
        }
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
                Token::string("hello", false),
                Token::Whitespace,
                Token::string("world[]{}():!,;=^|<||>->", true),
                Token::Whitespace,
                Token::string("escape\"quote\n", true),
            ],
        )
    }

    #[test]
    fn raw_strings() -> Result<(), Source<Error>> {
        assert_tokens(
            r"'hello \\\n world'",
            &[Token::string(r"hello \\\n world", true)],
        )?;
        assert_tokens(
            r#"'''with ' quotes '' "123'''"#,
            &[Token::string(r#"with ' quotes '' "123"#, true)],
        )?;
        Ok(())
    }

    #[test]
    fn quote_concat_strings() -> Result<(), Source<Error>> {
        assert_tokens("hello\"world\"'hi'", &[Token::string("helloworldhi", true)])
    }

    #[test]
    fn string_ends() -> Result<(), Source<Error>> {
        assert_tokens(
            "a[b]c{d}e(f)g:h,i;j^k=l\nm n|o<|p|>q!r->",
            &[
                Token::string("a", false),
                Token::open_bracket(),
                Token::string("b", false),
                Token::close_bracket(),
                Token::string("c", false),
                Token::open_curly(),
                Token::string("d", false),
                Token::close_curly(),
                Token::string("e", false),
                Token::open_paren(),
                Token::string("f", false),
                Token::close_paren(),
                Token::string("g", false),
                Token::colon(),
                Token::string("h", false),
                Token::Comma,
                Token::string("i", false),
                Token::Semicolon,
                Token::string("j", false),
                Token::caret(),
                Token::string("k", false),
                Token::equal(),
                Token::string("l", false),
                Token::Newline,
                Token::string("m", false),
                Token::Whitespace,
                Token::string("n", false),
                Token::pipe(),
                Token::string("o", false),
                Token::pipe_left(),
                Token::string("p", false),
                Token::pipe_right(),
                Token::string("q", false),
                Token::bang(),
                Token::string("r", false),
                Token::arrow(),
            ],
        )
    }

    #[test]
    fn unicode_escape() -> Result<(), Source<Error>> {
        assert_tokens(r#""my \u{65}scape""#, &[Token::string("my escape", true)])
    }

    #[test]
    fn invalid_unicode_escape() {
        let err = assert_tokens(r#""\u{FFFFFF}""#, &[]).unwrap_err().unwrap();
        assert!(err == Error::InvalidUnicodeCharacter);

        let err = assert_tokens(r#""\u{hi}""#, &[]).unwrap_err().unwrap();
        assert!(err == Error::InvalidUnicodeCharacter);

        let err = assert_tokens(r#""\u{}""#, &[]).unwrap_err().unwrap();
        assert!(err == Error::InvalidUnicodeCharacter);
    }

    #[test]
    fn unclosed_unicode_escape() {
        let err = assert_tokens(r#""\u{1234""#, &[]).unwrap_err().unwrap();
        println!("{}", err);
        assert!(err == Error::UnfinishedUnicodeCharacter);
    }

    #[test]
    fn long_unicode_escape() {
        let err = assert_tokens(r#""\u{1234512345}""#, &[])
            .unwrap_err()
            .unwrap();
        assert!(err == Error::LongUnicodeCharacter);
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
    fn line_comments() -> Result<(), Source<Error>> {
        assert_tokens(
            "# This is a comment\n: # This is also a comment\n# One last comment",
            &[
                Token::Newline,
                Token::colon(),
                Token::Whitespace,
                Token::Newline,
            ],
        )
    }

    #[test]
    fn tree_comments() -> Result<(), Source<Error>> {
        assert_tokens(
            "#hello world",
            &[
                Token::TreeComment,
                Token::string("hello", false),
                Token::Whitespace,
                Token::string("world", false),
            ],
        )?;
        assert_tokens(
            "#[hello world]",
            &[
                Token::TreeComment,
                Token::open_bracket(),
                Token::string("hello", false),
                Token::Whitespace,
                Token::string("world", false),
                Token::close_bracket(),
            ],
        )?;
        Ok(())
    }

    #[test]
    fn doc_comments() -> Result<(), Source<Error>> {
        assert_tokens(
            "##This is a doc comment\nhello world",
            &[
                Token::DocComment,
                Token::doc_string("This is a doc comment"),
                Token::Newline,
                Token::string("hello", false),
                Token::Whitespace,
                Token::string("world", false),
            ],
        )?;
        assert_tokens(
            "##This is a doc comment\n##more doc comment\n  ##  and more\nhello",
            &[
                Token::DocComment,
                Token::doc_string("This is a doc comment"),
                Token::Newline,
                Token::DocComment,
                Token::doc_string("more doc comment"),
                Token::Newline,
                Token::Whitespace,
                Token::DocComment,
                Token::doc_string("  and more"),
                Token::Newline,
                Token::string("hello", false),
            ],
        )?;
        Ok(())
    }

    #[test]
    fn doc_comment_expression() -> Result<(), Source<Error>> {
        assert_tokens(
            "##doc comment {{ hello }}",
            &[
                Token::DocComment,
                Token::doc_string("doc comment "),
                Token::open_doc_curly(),
                Token::Whitespace,
                Token::string("hello", false),
                Token::Whitespace,
                Token::close_doc_curly(),
                Token::doc_string(""),
            ],
        )?;
        assert_tokens(
            "##doc comment {{\n##hello\n##}}",
            &[
                Token::DocComment,
                Token::doc_string("doc comment "),
                Token::open_doc_curly(),
                Token::Newline,
                Token::DocComment,
                Token::string("hello", false),
                Token::Newline,
                Token::DocComment,
                Token::close_doc_curly(),
                Token::doc_string(""),
            ],
        )?;
        Ok(())
    }

    fn assert_tokens(s: &str, expected: &[Token]) -> Result<(), Source<Error>> {
        let toks: Vec<_> = Tokens::from(Source::missing(s.chars()))
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .map(|t| t.unwrap())
            .collect();
        dbg!(&toks);
        assert!(toks == expected);
        Ok(())
    }
}
