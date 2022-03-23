//! AST tokenization.
//!
//! This does more than "traditional" tokenization, incorporating some parts of what might be
//! considered parsing to have context-aware tokenization.
//!
//! The output stream of tokens is whitespace-agnostic. Commas and semicolons interpreted as
//! expression separators where relevant (within curly and bracket tokens). Specifically,
//! whitespace and commas/semicolons are interpreted as follows:
//!
//! * Lines (the root of the parsing) may contain whitespace-separated token trees.
//! * Parens may contain whitespace- and/or newline-separated token trees.
//! * Curly brackets and square brackets may contain newline-, semicolon-, and/or comma-separated
//! lines.
//!
//! Quotes and line-oriented blocks preceded by apostrophes/hash/double-hash followed by a space
//! are considered token trees. All but hash-preceded trees may contain carets which each must be
//! followed by a single token tree.

use ergo_runtime::abi_stable::type_erase::{Eraseable, Erased};
use ergo_runtime::source::Source;
use std::fmt;

/// Tokens relevant to expression interpretation.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SymbolicToken {
    Equal,
    Caret,
    Colon,
    ColonPrefix,
    Bang,
    Arrow,
    Pipe,
    PipeLeft,
    PipeRight,
    Hash,
    DoubleHash,
}

/// Tokens which are parsed in pairs and used in grouping other tokens.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PairedToken {
    Paren,
    Curly,
    Bracket,
    Quote,
    ApostropheSpace,
    HashSpace,
    DoubleHashSpace,
}

/// Script tokens.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token<'a> {
    /// A string token.
    ///
    /// If strings are within quotes, they are guaranteed to have at least one character after any
    /// backslash characters.
    String(&'a str),
    /// A symbolic token.
    Symbol(SymbolicToken),
    /// Start a nested paired grouping.
    StartNested(PairedToken),
    /// End the most recent nested paired grouping.
    EndNested,
    /// Start the next child withing the current grouping.
    ///
    /// Only valid within PairedToken::Curly and PairedToken::Bracket.
    NextChild,
}

impl<'a> Token<'a> {
    pub fn string<S: Into<&'a str>>(s: S) -> Self {
        Token::String(s.into())
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

    pub fn colon_prefix() -> Self {
        Token::Symbol(SymbolicToken::ColonPrefix)
    }

    pub fn bang() -> Self {
        Token::Symbol(SymbolicToken::Bang)
    }

    pub fn arrow() -> Self {
        Token::Symbol(SymbolicToken::Arrow)
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

    pub fn hash() -> Self {
        Token::Symbol(SymbolicToken::Hash)
    }

    pub fn double_hash() -> Self {
        Token::Symbol(SymbolicToken::DoubleHash)
    }

    pub fn paren() -> Self {
        Token::StartNested(PairedToken::Paren)
    }

    pub fn curly() -> Self {
        Token::StartNested(PairedToken::Curly)
    }

    pub fn bracket() -> Self {
        Token::StartNested(PairedToken::Bracket)
    }

    pub fn quote() -> Self {
        Token::StartNested(PairedToken::Quote)
    }

    pub fn apostrophe_space() -> Self {
        Token::StartNested(PairedToken::ApostropheSpace)
    }

    pub fn hash_space() -> Self {
        Token::StartNested(PairedToken::HashSpace)
    }

    pub fn double_hash_space() -> Self {
        Token::StartNested(PairedToken::DoubleHashSpace)
    }

    pub fn close() -> Self {
        Token::EndNested
    }

    pub fn next() -> Self {
        Token::NextChild
    }
}

impl fmt::Display for SymbolicToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use SymbolicToken::*;
        match self {
            Equal => write!(f, "="),
            Caret => write!(f, "^"),
            Colon | ColonPrefix => write!(f, ":"),
            Bang => write!(f, "!"),
            Arrow => write!(f, "->"),
            Pipe => write!(f, "|"),
            PipeLeft => write!(f, "<|"),
            PipeRight => write!(f, "|>"),
            Hash => write!(f, "#"),
            DoubleHash => write!(f, "##"),
        }
    }
}

impl fmt::Display for PairedToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use PairedToken::*;
        match self {
            Paren => write!(f, "parenthesis"),
            Curly => write!(f, "curly bracket"),
            Bracket => write!(f, "square bracket"),
            Quote => write!(f, "quote"),
            ApostropheSpace => write!(f, "block string"),
            HashSpace => write!(f, "line comment"),
            DoubleHashSpace => write!(f, "doc comment"),
        }
    }
}

impl PartialEq<Source<Self>> for PairedToken {
    fn eq(&self, other: &Source<Self>) -> bool {
        self == &**other
    }
}

impl<'a> PartialEq<Source<Self>> for Token<'a> {
    fn eq(&self, other: &Source<Self>) -> bool {
        self == &**other
    }
}

/// Tokenization errors.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error {
    /// A block string had an invalid starting sequence.
    InvalidBlockString,
    /// A colon was in an invalid location.
    InvalidColon,
    /// A caret within a quoted string was not followed by a word.
    InvalidStringCaret,
    /// An expression separator is invalid (comma or semicolon within nested expression).
    InvalidExpressionSeparator,
    /// An opened PairedToken does not have a corresponding closing PairedToken.
    UnmatchedOpeningToken(PairedToken),
    /// A closing PairedToken does not match the most recently-opened PairedToken.
    UnmatchedClosingToken(PairedToken, Option<Source<PairedToken>>),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::InvalidBlockString => write!(f, "block strings must be started with `\' `"),
            Error::InvalidColon => write!(
                f,
                "colons must always be followed by an expression (without whitespace)"
            ),
            Error::InvalidStringCaret => write!(f, "expected an alphanumeric string to follow the caret"),
            Error::InvalidExpressionSeparator => write!(f, "commas and semicolons may only be within curly and square brackets to separate expressions"),
            Error::UnmatchedOpeningToken(t) => write!(f, "unmatched {}", t),
            Error::UnmatchedClosingToken(t, _) => write!(f, "unmatched {}", t),
        }
    }
}

impl std::error::Error for Error {}

mod next_token {
    use super::*;

    pub(super) struct Ref<'a, T> {
        next: &'a mut Next,
        valid: bool,
        _phantom: std::marker::PhantomData<&'a T>,
    }

    impl<'a, T> Ref<'a, T> {
        pub fn as_ref(&self) -> &T {
            assert!(self.valid);
            unsafe { self.next.current().as_ref() }
        }

        pub fn as_mut(&mut self) -> &mut T {
            assert!(self.valid);
            unsafe { self.next.current_mut().as_mut() }
        }

        pub fn push<N: NextToken>(&mut self, next: N) {
            self.next.push(next);
            self.valid = false;
        }

        pub fn finish(&mut self) -> T {
            assert!(self.valid);
            self.valid = false;
            unsafe { self.next.pop().to_owned() }
        }
    }

    pub(super) trait NextToken: Eraseable + Sized {
        fn next<'a, 'tok>(
            this: &mut Ref<'a, Self>,
            position: &mut TokenPosition<'tok>,
        ) -> Option<Result<Source<Token<'tok>>, Source<Error>>>;

        fn call_next<'tok>(
            next: &mut Next,
            position: &mut TokenPosition<'tok>,
        ) -> Option<Result<Source<Token<'tok>>, Source<Error>>> {
            let mut this = Ref {
                next,
                valid: true,
                _phantom: std::marker::PhantomData,
            };
            Self::next(&mut this, position)
        }

        fn close(self) -> Result<usize, Source<Error>>;

        fn call_close(e: Erased) -> Result<usize, Source<Error>> {
            Self::close(unsafe { e.to_owned() })
        }
    }

    struct Impl {
        call_next: for<'tok> fn(
            &mut Next,
            &mut TokenPosition<'tok>,
        ) -> Option<Result<Source<Token<'tok>>, Source<Error>>>,
        call_close: fn(Erased) -> Result<usize, Source<Error>>,
        data: Erased,
    }

    impl Impl {
        pub fn new<T: NextToken>(imp: T) -> Self {
            Impl {
                call_next: T::call_next,
                call_close: T::call_close,
                data: Erased::new(imp),
            }
        }

        pub unsafe fn as_ref<T>(&self) -> &T {
            self.data.as_ref()
        }

        pub unsafe fn as_mut<T>(&mut self) -> &mut T {
            self.data.as_mut()
        }

        pub unsafe fn to_owned<T>(self) -> T {
            self.data.to_owned()
        }
    }

    pub(super) struct Next(Vec<Impl>);

    impl Next {
        pub fn empty() -> Self {
            Next(vec![])
        }

        pub fn new<T: NextToken>(initial: T) -> Self {
            Next(vec![Impl::new(initial)])
        }

        pub fn next<'tok>(
            &mut self,
            position: &mut TokenPosition<'tok>,
        ) -> Option<Result<Source<Token<'tok>>, Source<Error>>> {
            if self.is_empty() {
                None
            } else {
                let impl_fn = self.current().call_next;
                impl_fn(self, position)
            }
        }

        pub fn is_empty(&self) -> bool {
            self.0.is_empty()
        }

        pub fn close<'tok>(&mut self) -> Result<usize, Source<Error>> {
            let mut total = 0;
            while let Some(imp) = self.0.pop() {
                total += (imp.call_close)(imp.data)?;
            }
            Ok(total)
        }

        pub fn push<T: NextToken>(&mut self, next: T) {
            self.0.push(Impl::new(next));
        }

        fn current(&self) -> &Impl {
            self.0.last().unwrap()
        }

        fn current_mut(&mut self) -> &mut Impl {
            self.0.last_mut().unwrap()
        }

        fn pop(&mut self) -> Impl {
            self.0.pop().unwrap()
        }
    }
}

use next_token::{Next, NextToken};

// All token parsing must be line-oriented, because of the use of line-prefixed contextual tokens.
// What this means is that e.g., a quoted string spanning over a newline will be split into two
// separate string tokens (which will be recombined with further quoted string processing).

enum TokenElseString {
    /// Return a token.
    Some(Token<'static>),
    /// Indicate the character is a string character.
    String,
    /// Move to the next character, but don't consider this a string character.
    NonString,
    /// Indicate an error.
    Err(Error),
}

/// Read a token, optionally preceded by a string. Thus we move to the next non-string token while
/// also parsing a string (if any) along the way.
fn token_else_string<'a, 'tok, T, F>(
    this: &mut next_token::Ref<'a, T>,
    position: &mut TokenPosition<'tok>,
    mut get_token: F,
) -> Option<Result<Source<Token<'tok>>, Source<Error>>>
where
    F: FnMut(char, &mut next_token::Ref<'a, T>, &mut TokenPosition<'tok>) -> TokenElseString,
{
    let mut pending_str = position.remaining;
    let mut pending_str_len = 0;

    let token = loop {
        position.reset_source_start();
        if let Some(c) = position.next_char() {
            match get_token(c, this, position) {
                TokenElseString::Err(e) => break Some(Err(position.source(e))),
                TokenElseString::Some(tok) => break Some(Ok(position.source(tok))),
                TokenElseString::NonString => {
                    if pending_str_len > 0 || c == '\n' {
                        break None;
                    } else {
                        pending_str = position.remaining;
                        continue;
                    }
                }
                TokenElseString::String => {
                    pending_str_len += c.len_utf8();
                }
            }
        } else {
            break None;
        }
    };

    if pending_str_len > 0 {
        if let Some(t) = token {
            this.push(ImmediateResult(t));
        }
        // SAFETY: pending_str_len is only incremented with the byte counts of subsequent
        // characters within pending_str itself.
        let s = unsafe { pending_str.get_unchecked(..pending_str_len) };
        let mut src = position.source.clone();
        src.location.start -= pending_str_len;
        src.location.length = pending_str_len;
        Some(Ok(src.with(Token::string(s))))
    } else {
        token
    }
}

trait FinishAt: Eraseable {
    const GROUP: bool;
}

macro_rules! finish_at {
    ( $name:ident, $group:literal ) => {
        struct $name;
        impl FinishAt for $name {
            const GROUP: bool = $group;
        }
    };
}

finish_at!(FinishAtNone, false);
finish_at!(FinishAtGroup, true);

/// Normal tokenization, with an optional (compile-time) behavior to finish after the first group
/// completes. This is used for string expression parsing.
struct Normal<T> {
    nested: Vec<Source<PairedToken>>,
    previous_was_boundary: bool,
    _finish_at: T,
}

impl<T> Normal<T> {
    fn new(finish_at: T) -> Self {
        Normal {
            nested: Default::default(),
            previous_was_boundary: true,
            _finish_at: finish_at,
        }
    }
}

impl<T: FinishAt> NextToken for Normal<T> {
    fn next<'a, 'tok>(
        this: &mut next_token::Ref<'a, Self>,
        position: &mut TokenPosition<'tok>,
    ) -> Option<Result<Source<Token<'tok>>, Source<Error>>> {
        let has_children = this
            .as_ref()
            .nested
            .last()
            .map(|t| t == &PairedToken::Bracket || t == &PairedToken::Curly)
            .unwrap_or(!T::GROUP);
        token_else_string(this, position, |c, this, position| {
            let me = this.as_mut();
            let previous_was_boundary =
                std::mem::replace(&mut me.previous_was_boundary, false) || position.line_start;
            TokenElseString::Some(match c {
                '=' => Token::equal(),
                '^' => Token::caret(),
                ':' => {
                    let next_is_boundary =
                        position.peek().map(|c| c.is_whitespace()).unwrap_or(true);
                    if next_is_boundary {
                        return TokenElseString::Err(Error::InvalidColon);
                    } else if previous_was_boundary {
                        Token::colon_prefix()
                    } else {
                        Token::colon()
                    }
                }
                '!' => Token::bang(),
                '-' if position.char_match(">") => Token::arrow(),
                '|' if position.char_match(">") => Token::pipe_right(),
                '|' => Token::pipe(),
                '<' if position.char_match("|") => Token::pipe_left(),
                '#' => {
                    let double_hash = position.char_match("#");
                    let has_space = position.char_match(" ")
                        || position.peek().map(|c| c == '\n').unwrap_or(true);

                    match (double_hash, has_space) {
                        (true, true) => {
                            this.push(BlockString::new(DoubleHashPrefix, has_children));
                            Token::double_hash_space()
                        }
                        (true, false) => Token::double_hash(),
                        (false, true) => {
                            this.push(BlockString::new(HashPrefix, has_children));
                            Token::hash_space()
                        }
                        (false, false) => Token::hash(),
                    }
                }
                '(' => {
                    me.nested.push(position.source(PairedToken::Paren));
                    Token::paren()
                }
                ')' => {
                    let last = me.nested.pop();
                    if let Some(&PairedToken::Paren) = last.as_ref().map(|t| &**t) {
                        if T::GROUP && me.nested.is_empty() {
                            this.finish();
                        }
                        Token::close()
                    } else {
                        return TokenElseString::Err(Error::UnmatchedClosingToken(
                            PairedToken::Paren,
                            last,
                        ));
                    }
                }
                '{' => {
                    me.nested.push(position.source(PairedToken::Curly));
                    Token::curly()
                }
                '}' => {
                    let last = me.nested.pop();
                    if let Some(&PairedToken::Curly) = last.as_ref().map(|t| &**t) {
                        if T::GROUP && me.nested.is_empty() {
                            this.finish();
                        }
                        Token::close()
                    } else {
                        return TokenElseString::Err(Error::UnmatchedClosingToken(
                            PairedToken::Curly,
                            last,
                        ));
                    }
                }
                '[' => {
                    me.nested.push(position.source(PairedToken::Bracket));
                    Token::bracket()
                }
                ']' => {
                    let last = me.nested.pop();
                    if let Some(&PairedToken::Bracket) = last.as_ref().map(|t| &**t) {
                        if T::GROUP && me.nested.is_empty() {
                            this.finish();
                        }
                        Token::close()
                    } else {
                        return TokenElseString::Err(Error::UnmatchedClosingToken(
                            PairedToken::Bracket,
                            last,
                        ));
                    }
                }
                '"' => {
                    if T::GROUP && me.nested.is_empty() {
                        this.finish();
                    }
                    this.push(QuoteString::new(position.source(())));
                    Token::quote()
                }
                '\'' => {
                    let has_space = position.char_match(" ")
                        || position.peek().map(|c| c == '\n').unwrap_or(true);
                    if has_space {
                        if T::GROUP && me.nested.is_empty() {
                            this.finish();
                        }
                        this.push(BlockString::new(ApostrophePrefix, has_children));
                        Token::apostrophe_space()
                    } else {
                        return TokenElseString::Err(Error::InvalidBlockString);
                    }
                }
                ',' | ';' | '\n' if has_children => {
                    me.previous_was_boundary = true;
                    Token::next()
                }
                ',' | ';' => return TokenElseString::Err(Error::InvalidExpressionSeparator),
                c if c.is_whitespace() => {
                    me.previous_was_boundary = true;
                    return TokenElseString::NonString;
                }
                _ => return TokenElseString::String,
            })
        })
    }

    fn close(mut self) -> Result<usize, Source<Error>> {
        if let Some(last) = self.nested.pop() {
            Err(last.map(Error::UnmatchedOpeningToken))
        } else {
            Ok(0)
        }
    }
}

/// Caret word tokenization, within a quoted string.
///
/// This tokenization context only reads a single alphanumeric and [-_] string.
struct StringCaretWord {
    caret_source: Source<()>,
}

impl StringCaretWord {
    fn new(caret_source: Source<()>) -> Self {
        StringCaretWord { caret_source }
    }
}

impl NextToken for StringCaretWord {
    fn next<'a, 'tok>(
        this: &mut next_token::Ref<'a, Self>,
        position: &mut TokenPosition<'tok>,
    ) -> Option<Result<Source<Token<'tok>>, Source<Error>>> {
        position.reset_source_start();

        // This will only run once.
        let me = this.finish();

        let end = position
            .remaining
            .find(|c: char| !c.is_alphanumeric() && !"-_".contains(c))
            .unwrap_or(position.remaining.len());
        Some(if end == 0 {
            Err(me.caret_source.with(Error::InvalidStringCaret))
        } else {
            let (s, rest) = position.remaining.split_at(end);
            position.remaining = rest;
            position.source.location.length += s.len();
            Ok(position.source(Token::string(s)))
        })
    }

    fn close(self) -> Result<usize, Source<Error>> {
        Err(self.caret_source.with(Error::InvalidStringCaret))
    }
}

/// A tokenization context which simply returns some number of closing tokens.
struct CloseTokens {
    count: usize,
    source: Source<()>,
}

impl CloseTokens {
    fn new(count: usize, source: Source<()>) -> Self {
        assert!(count > 0);
        CloseTokens { count, source }
    }
}

impl NextToken for CloseTokens {
    fn next<'a, 'tok>(
        this: &mut next_token::Ref<'a, Self>,
        _position: &mut TokenPosition<'tok>,
    ) -> Option<Result<Source<Token<'tok>>, Source<Error>>> {
        this.as_mut().count -= 1;

        let source = if this.as_ref().count == 0 {
            this.finish().source
        } else {
            this.as_ref().source.clone()
        };
        Some(Ok(source.with(Token::close())))
    }

    fn close(self) -> Result<usize, Source<Error>> {
        Ok(self.count)
    }
}

fn string_expression<'a, F>(position: &mut TokenPosition<'a>, set_next: F) -> Token<'static>
where
    F: FnOnce(bool),
{
    if position.char_match("^") {
        Token::string("^")
    } else {
        match position.peek() {
            Some('(' | '[' | '{' | '"') => set_next(true),
            _ => set_next(false),
        }
        Token::caret()
    }
}

trait BlockStringPrefix: Eraseable {
    const DELIMITER: &'static str;
    const ALLOW_NESTED: bool;
}

macro_rules! block_string_prefix {
    ($name:ident, $delimiter:literal, $allow_nested:literal) => {
        struct $name;
        impl BlockStringPrefix for $name {
            const DELIMITER: &'static str = $delimiter;
            const ALLOW_NESTED: bool = $allow_nested;
        }
    };
}

block_string_prefix!(HashPrefix, "#", false);
block_string_prefix!(DoubleHashPrefix, "##", true);
block_string_prefix!(ApostrophePrefix, "'", true);

/// A tokenization context which reads a block string of some sort. This includes doc comment
/// blocks, string blocks, and comment blocks.
struct BlockString<Prefix> {
    nested: Next,
    end_with_next: bool,
    _prefix: Prefix,
}

impl<Prefix> BlockString<Prefix> {
    pub fn new(prefix: Prefix, end_with_next: bool) -> Self {
        BlockString {
            nested: Next::empty(),
            end_with_next,
            _prefix: prefix,
        }
    }
}

impl<T: BlockStringPrefix> NextToken for BlockString<T> {
    fn next<'a, 'tok>(
        this: &mut next_token::Ref<'a, Self>,
        position: &mut TokenPosition<'tok>,
    ) -> Option<Result<Source<Token<'tok>>, Source<Error>>> {
        let nested = &mut this.as_mut().nested;

        if position.line_start {
            // Skip non-newline whitespace and look for delimiter
            let s = position
                .remaining
                .trim_start_matches(|c: char| c.is_whitespace() && c != '\n');
            match s.strip_prefix(T::DELIMITER) {
                Some(mut rest) if rest.starts_with(&[' ', '\n'][..]) => {
                    // match delimiter, continue with this tokenization

                    if rest.starts_with(' ') {
                        // SAFETY: starts_with guarantees we can skip forward a space
                        rest = unsafe { rest.get_unchecked(' '.len_utf8()..) };
                    }
                    position.reset_source_start();
                    position.source.location.length += position.remaining.len() - rest.len();
                    position.reset_source_start();
                    position.remaining = rest;
                }
                _ => {
                    // no delimiter, finish
                    // flush inner tokens
                    if let Some(r) = nested.next(position) {
                        return Some(r);
                    }
                    let mut me = this.finish();
                    return Some(me.nested.close().map(|close_tokens| {
                        if me.end_with_next {
                            this.push(ImmediateResult(Ok(position.source(Token::next()))));
                        }
                        if close_tokens > 0 {
                            this.push(CloseTokens::new(close_tokens, position.source(())));
                        }
                        position.source(Token::close())
                    }));
                }
            }
        }

        if nested.is_empty() {
            token_else_string(this, position, |c, this, position| {
                TokenElseString::Some(match c {
                    '^' if T::ALLOW_NESTED => {
                        let source = position.source(());
                        string_expression(position, |next_group| {
                            this.as_mut().nested = if next_group {
                                Next::new(Normal::new(FinishAtGroup))
                            } else {
                                Next::new(StringCaretWord::new(source))
                            };
                        })
                    }
                    _ => return TokenElseString::String,
                })
            })
        } else {
            nested.next(position)
        }
    }

    fn close(mut self) -> Result<usize, Source<Error>> {
        Ok(self.nested.close()? + 1)
    }
}

/// A tokenization context which reads a string until (and including) the next newline.
struct LineString;

impl NextToken for LineString {
    fn next<'a, 'tok>(
        this: &mut next_token::Ref<'a, Self>,
        position: &mut TokenPosition<'tok>,
    ) -> Option<Result<Source<Token<'tok>>, Source<Error>>> {
        let ret = token_else_string(this, position, |_, _, _| TokenElseString::String);
        this.finish();
        ret
    }

    fn close(self) -> Result<usize, Source<Error>> {
        panic!("line string closing");
    }
}

/// A tokenization context which reads a quoted string, with similar tokenization to block strings.
struct QuoteString {
    start: Source<()>,
}

impl QuoteString {
    fn new(start: Source<()>) -> Self {
        QuoteString { start }
    }
}

impl NextToken for QuoteString {
    fn next<'a, 'tok>(
        this: &mut next_token::Ref<'a, Self>,
        position: &mut TokenPosition<'tok>,
    ) -> Option<Result<Source<Token<'tok>>, Source<Error>>> {
        let mut escaped = false;
        token_else_string(this, position, |c, this, position| {
            if std::mem::replace(&mut escaped, false) {
                TokenElseString::String
            } else {
                TokenElseString::Some(match c {
                    '^' => {
                        let source = position.source(());
                        string_expression(position, |next_group| {
                            if next_group {
                                this.push(Normal::new(FinishAtGroup));
                            } else {
                                this.push(StringCaretWord::new(source));
                            }
                        })
                    }
                    '"' => {
                        this.finish();
                        Token::close()
                    }
                    c => {
                        escaped = c == '\\';
                        return TokenElseString::String;
                    }
                })
            }
        })
    }

    fn close(self) -> Result<usize, Source<Error>> {
        Err(self
            .start
            .with(Error::UnmatchedOpeningToken(PairedToken::Quote)))
    }
}

/// A tokenization context which returns a single result immediately.
struct ImmediateResult(Result<Source<Token<'static>>, Source<Error>>);

impl NextToken for ImmediateResult {
    fn next<'a, 'tok>(
        this: &mut next_token::Ref<'a, Self>,
        _position: &mut TokenPosition<'tok>,
    ) -> Option<Result<Source<Token<'tok>>, Source<Error>>> {
        Some(this.finish().0)
    }

    fn close(self) -> Result<usize, Source<Error>> {
        panic!("immediate result closing: {:?}", self.0);
    }
}

#[derive(Debug)]
struct TokenPosition<'a> {
    remaining: &'a str,
    source: Source<()>,
    line_start: bool,
    next_line_start: bool,
}

/// Iterator producing tokens or errors.
pub struct Tokens<'a> {
    position: TokenPosition<'a>,
    next: Next,
}

impl<'a> From<Source<&'a str>> for Tokens<'a> {
    fn from(s: Source<&'a str>) -> Self {
        let (source, remaining) = s.take();
        let mut position = TokenPosition {
            remaining,
            source,
            line_start: true,
            next_line_start: false,
        };
        let mut next = Next::new(Normal::new(FinishAtNone));

        // Check for shebang at beginning of script and handle it specially.
        if position.remaining.starts_with("#!") {
            position.next_char().unwrap();
            let hash = ImmediateResult(Ok(position.source(Token::hash())));
            position.reset_source_start();
            position.next_char().unwrap();
            let bang = ImmediateResult(Ok(position.source(Token::bang())));
            position.reset_source_start();
            // Emit hash+bang+string, which will be ignored as a tree comment (but can be preserved
            // if rewriting the token stream).
            next.push(LineString);
            next.push(bang);
            next.push(hash);
        }

        Tokens { position, next }
    }
}

impl<'a> TokenPosition<'a> {
    pub fn next_char(&mut self) -> Option<char> {
        if self.next_line_start {
            self.next_line_start = false;
            self.line_start = true;
            return None;
        }
        let mut chars = self.remaining.chars();
        let c = chars.next()?;
        self.remaining = chars.as_str();
        self.source.location.length += c.len_utf8();
        self.next_line_start = c == '\n';
        self.line_start = false;
        Some(c)
    }

    pub fn peek(&self) -> Option<char> {
        self.remaining.chars().next()
    }

    pub fn char_match(&mut self, s: &str) -> bool {
        if self.remaining.starts_with(s) {
            // SAFETY: `s.len()` must be a valid unicode-boundary offset of self.remaining
            self.remaining = unsafe { self.remaining.get_unchecked(s.len()..) };
            self.source.location.length += s.len();
            true
        } else {
            false
        }
    }

    pub fn reset_source_start(&mut self) {
        self.source.location.start += self.source.location.length;
        self.source.location.length = 0;
    }

    pub fn source<T>(&self, v: T) -> Source<T> {
        self.source.clone().with(v)
    }
}

impl<'a> Iterator for Tokens<'a> {
    type Item = Result<Source<Token<'a>>, Source<Error>>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let result = self.next.next(&mut self.position);
            match result {
                Some(v) => break Some(v),
                None => {
                    if self.position.remaining.is_empty() {
                        break match self.next.close() {
                            Err(e) => Some(Err(e)),
                            Ok(close_tokens) => {
                                if close_tokens > 1 {
                                    self.next = Next::new(CloseTokens::new(
                                        close_tokens - 1,
                                        self.position.source(()),
                                    ));
                                }
                                if close_tokens > 0 {
                                    Some(Ok(self.position.source(Token::close())))
                                } else {
                                    None
                                }
                            }
                        };
                    } else {
                        continue;
                    }
                }
            }
        }
    }
}
