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

use ergo_runtime::source::Source;
use std::fmt;

#[macro_export]
macro_rules! StringTypeSlice {
    ( $t:ident < $l:lifetime > ) => {
        <$t as StringTypeT<$l>>::Slice
    };
}

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

/// Tokens which are used to continue grouped blocks.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LeaderToken {
    Apostrophe,
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
    Apostrophe,
    Hash,
    DoubleHash,
}

/// Script tokens.
#[derive(Clone, Debug, Eq)]
pub enum Token<T> {
    /// A string token.
    ///
    /// If strings are within quotes, they are guaranteed to have at least one character after any
    /// backslash characters.
    String(T),
    /// A symbolic token.
    Symbol(SymbolicToken),
    /// A block continuation token.
    Leader(LeaderToken),
    /// Start a nested paired grouping.
    StartNested(PairedToken),
    /// End the most recent nested paired grouping.
    EndNested,
    /// Start the next child within the current grouping.
    ///
    /// Only valid within PairedToken::Curly and PairedToken::Bracket.
    NextChild,
}

impl<T> Token<T> {
    pub fn string<S: Into<T>>(s: S) -> Self {
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

    pub fn leading_apostrophe() -> Self {
        Token::Leader(LeaderToken::Apostrophe)
    }

    pub fn leading_hash() -> Self {
        Token::Leader(LeaderToken::Hash)
    }

    pub fn leading_double_hash() -> Self {
        Token::Leader(LeaderToken::DoubleHash)
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
        Token::StartNested(PairedToken::Apostrophe)
    }

    pub fn hash_space() -> Self {
        Token::StartNested(PairedToken::Hash)
    }

    pub fn double_hash_space() -> Self {
        Token::StartNested(PairedToken::DoubleHash)
    }

    pub fn close() -> Self {
        Token::EndNested
    }

    pub fn next() -> Self {
        Token::NextChild
    }

    pub fn map<U, F: FnMut(T) -> U>(self, mut f: F) -> Token<U> {
        match self {
            Token::String(s) => Token::String(f(s)),
            Token::Symbol(s) => Token::Symbol(s),
            Token::Leader(s) => Token::Leader(s),
            Token::StartNested(s) => Token::StartNested(s),
            Token::EndNested => Token::EndNested,
            Token::NextChild => Token::NextChild,
        }
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

impl fmt::Display for LeaderToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use LeaderToken::*;
        match self {
            Apostrophe => write!(f, "'"),
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
            Apostrophe => write!(f, "block string"),
            Hash => write!(f, "line comment"),
            DoubleHash => write!(f, "doc comment"),
        }
    }
}

impl PartialEq<Source<Self>> for PairedToken {
    fn eq(&self, other: &Source<Self>) -> bool {
        self == &**other
    }
}

impl<U, T: PartialEq<U>> PartialEq<Token<U>> for Token<T> {
    fn eq(&self, other: &Token<U>) -> bool {
        match (self, other) {
            (Token::String(t), Token::String(u)) => t == u,
            (Token::Symbol(a), Token::Symbol(b)) => a == b,
            (Token::Leader(a), Token::Leader(b)) => a == b,
            (Token::StartNested(a), Token::StartNested(b)) => a == b,
            (Token::EndNested, Token::EndNested) => true,
            (Token::NextChild, Token::NextChild) => true,
            _ => false,
        }
    }
}

impl<U, T: PartialEq<U>> PartialEq<Source<Token<U>>> for Token<T> {
    fn eq(&self, other: &Source<Token<U>>) -> bool {
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

impl super::ToDiagnostic for Error {
    fn additional_info(&self, diagnostic: &mut ergo_runtime::error::Diagnostic) {
        match self {
            Error::UnmatchedClosingToken(_, Some(close)) => {
                diagnostic
                    .labels
                    .push(ergo_runtime::error::Label::secondary(
                        close.clone().map(|t| format!("expected match for {}", t)),
                    ));
            }
            _ => (),
        }
    }
}

mod next_token {
    use super::*;
    use ergo_runtime::abi_stable::{
        marker_type::UnsyncUnsend,
        type_erase::{EraseableT, ErasedT},
    };

    pub(super) struct Ref<'a, 'tok, S, T> {
        next: &'a mut Next<'tok, S>,
        valid: bool,
        _phantom: std::marker::PhantomData<&'a T>,
    }

    impl<'a, 'tok, S, T> Ref<'a, 'tok, S, T> {
        pub fn as_ref(&self) -> &T {
            assert!(self.valid);
            unsafe { self.next.current().as_ref() }
        }

        pub fn as_mut(&mut self) -> &mut T {
            assert!(self.valid);
            unsafe { self.next.current_mut().as_mut() }
        }

        pub fn push<N: NextToken<'tok, S>>(&mut self, next: N) {
            self.next.push(next);
            self.valid = false;
        }

        pub fn finish(&mut self) -> T {
            assert!(self.valid);
            self.valid = false;
            unsafe { self.next.pop().to_owned() }
        }
    }

    pub(super) trait NextToken<'tok, S>: EraseableT<'tok, UnsyncUnsend> + Sized {
        fn next<'a>(
            this: &mut Ref<'a, 'tok, S, Self>,
            position: &mut TokenPosition<S>,
        ) -> Option<Result<Source<Token<S>>, Source<Error>>>;

        fn call_next(
            next: &mut Next<'tok, S>,
            position: &mut TokenPosition<S>,
        ) -> Option<Result<Source<Token<S>>, Source<Error>>> {
            let mut this = Ref {
                next,
                valid: true,
                _phantom: std::marker::PhantomData,
            };
            Self::next(&mut this, position)
        }

        fn close(self) -> Result<usize, Source<Error>>;

        fn call_close(e: ErasedT<'tok, UnsyncUnsend>) -> Result<usize, Source<Error>> {
            Self::close(unsafe { e.to_owned() })
        }
    }

    struct Impl<'tok, S> {
        call_next: fn(
            &mut Next<'tok, S>,
            &mut TokenPosition<S>,
        ) -> Option<Result<Source<Token<S>>, Source<Error>>>,
        call_close: fn(ErasedT<'tok, UnsyncUnsend>) -> Result<usize, Source<Error>>,
        data: ErasedT<'tok, UnsyncUnsend>,
    }

    impl<'tok, S> Impl<'tok, S> {
        pub fn new<T: NextToken<'tok, S> + 'tok>(imp: T) -> Self {
            Impl {
                call_next: T::call_next,
                call_close: T::call_close,
                data: ErasedT::new(imp),
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

    pub(super) struct Next<'tok, S>(Vec<Impl<'tok, S>>);

    impl<'tok, S> Next<'tok, S> {
        pub fn empty() -> Self {
            Next(vec![])
        }

        pub fn new<T: NextToken<'tok, S>>(initial: T) -> Self {
            Next(vec![Impl::new(initial)])
        }

        pub fn next(
            &mut self,
            position: &mut TokenPosition<S>,
        ) -> Option<Result<Source<Token<S>>, Source<Error>>> {
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

        pub fn close(&mut self) -> Result<usize, Source<Error>> {
            let mut total = 0;
            while let Some(imp) = self.0.pop() {
                total += (imp.call_close)(imp.data)?;
            }
            Ok(total)
        }

        pub fn push<T: NextToken<'tok, S>>(&mut self, next: T) {
            self.0.push(Impl::new(next));
        }

        fn current(&self) -> &Impl<'tok, S> {
            self.0.last().unwrap()
        }

        fn current_mut(&mut self) -> &mut Impl<'tok, S> {
            self.0.last_mut().unwrap()
        }

        fn pop(&mut self) -> Impl<'tok, S> {
            self.0.pop().unwrap()
        }
    }
}

use next_token::{Next, NextToken};

// All token parsing must be line-oriented, because of the use of line-prefixed contextual tokens.
// What this means is that e.g., a quoted string spanning over a newline will be split into two
// separate string tokens (which will be recombined with further quoted string processing).

enum TokenElseString<S> {
    /// Return a token.
    Some(Token<S>),
    /// Indicate the character is a string character.
    String,
    /// Move to the next character, but don't consider this a string character.
    NonString,
    /// Indicate an error.
    Err(Error),
}

/// Read a token, optionally preceded by a string. Thus we move to the next non-string token while
/// also parsing a string (if any) along the way.
fn token_else_string<'a, 'tok, S: StringSlice<'tok>, T, F>(
    this: &mut next_token::Ref<'a, 'tok, S, T>,
    position: &mut TokenPosition<S>,
    mut get_token: F,
) -> Option<Result<Source<Token<S>>, Source<Error>>>
where
    F: FnMut(
        char,
        &mut next_token::Ref<'a, 'tok, S, T>,
        &mut TokenPosition<S>,
    ) -> TokenElseString<S>,
{
    let mut pending_str = position.remaining.clone();
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
                        pending_str = position.remaining.clone();
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
        let s = unsafe { pending_str.slice_to(pending_str_len) };
        let mut src = position.source.clone();
        src.location.start -= pending_str_len;
        src.location.length = pending_str_len;
        Some(Ok(src.with(Token::string(s))))
    } else {
        token
    }
}

trait FinishAt {
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

impl<'tok, T: FinishAt + 'tok, S: StringSlice<'tok>> NextToken<'tok, S> for Normal<T> {
    fn next<'a>(
        this: &mut next_token::Ref<'a, 'tok, S, Self>,
        position: &mut TokenPosition<S>,
    ) -> Option<Result<Source<Token<S>>, Source<Error>>> {
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
                '-' if position.match_skip(">") => Token::arrow(),
                '|' if position.match_skip(">") => Token::pipe_right(),
                '|' => Token::pipe(),
                '<' if position.match_skip("|") => Token::pipe_left(),
                '#' => {
                    let double_hash = position.match_skip("#");
                    let has_space = position.match_skip(" ")
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
                    let has_space = position.match_skip(" ")
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

impl<'tok, S: StringSlice<'tok>> NextToken<'tok, S> for StringCaretWord {
    fn next<'a>(
        this: &mut next_token::Ref<'a, 'tok, S, Self>,
        position: &mut TokenPosition<S>,
    ) -> Option<Result<Source<Token<S>>, Source<Error>>> {
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

impl<'tok, S: StringSlice<'tok>> NextToken<'tok, S> for CloseTokens {
    fn next<'a>(
        this: &mut next_token::Ref<'a, 'tok, S, Self>,
        _position: &mut TokenPosition<S>,
    ) -> Option<Result<Source<Token<S>>, Source<Error>>> {
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

fn string_expression<'a, 'tok, S: StringSlice<'tok>, F>(
    position: &mut TokenPosition<S>,
    set_next: F,
) -> Token<S>
where
    F: FnOnce(bool),
{
    if position.match_skip("^") {
        Token::string(S::from_str("^"))
    } else {
        match position.peek() {
            Some('(' | '[' | '{' | '"') => set_next(true),
            _ => set_next(false),
        }
        Token::caret()
    }
}

trait BlockStringPrefix {
    const DELIMITER: &'static str;
    const LEADER: LeaderToken;
    const ALLOW_NESTED: bool;
}

macro_rules! block_string_prefix {
    ($name:ident, $leader:ident, $str:literal, $allow_nested:literal) => {
        struct $name;
        impl BlockStringPrefix for $name {
            const DELIMITER: &'static str = $str;
            const LEADER: LeaderToken = LeaderToken::$leader;
            const ALLOW_NESTED: bool = $allow_nested;
        }
    };
}

block_string_prefix!(HashPrefix, Hash, "#", false);
block_string_prefix!(DoubleHashPrefix, DoubleHash, "##", true);
block_string_prefix!(ApostrophePrefix, Apostrophe, "'", true);

/// A tokenization context which reads a block string of some sort. This includes doc comment
/// blocks, string blocks, and comment blocks.
struct BlockString<'tok, S, Prefix> {
    nested: Next<'tok, S>,
    end_with_next: bool,
    set_line_start: bool,
    _prefix: Prefix,
}

impl<'tok, S, Prefix> BlockString<'tok, S, Prefix> {
    pub fn new(prefix: Prefix, end_with_next: bool) -> Self {
        BlockString {
            nested: Next::empty(),
            end_with_next,
            set_line_start: false,
            _prefix: prefix,
        }
    }
}

impl<'tok, T: BlockStringPrefix + 'tok, S: StringSlice<'tok>> NextToken<'tok, S>
    for BlockString<'tok, S, T>
{
    fn next<'a>(
        this: &mut next_token::Ref<'a, 'tok, S, Self>,
        position: &mut TokenPosition<S>,
    ) -> Option<Result<Source<Token<S>>, Source<Error>>> {
        let me = this.as_mut();
        let set_line_start = &mut me.set_line_start;
        let nested = &mut me.nested;

        if std::mem::replace(&mut position.line_start, std::mem::take(set_line_start)) {
            // Skip non-newline whitespace and look for delimiter
            let mut s = position
                .remaining
                .trim_start_matches(|c: char| c.is_whitespace() && c != '\n');
            if s.match_skip(T::DELIMITER) && (s.starts_with_char(' ') || s.starts_with_char('\n')) {
                // match delimiter, continue with this tokenization
                //
                position.reset_source_start();
                position.source.location.start +=
                    position.remaining.len() - s.len() - T::DELIMITER.len();
                position.source.location.length = T::DELIMITER.len();
                let ret = position.source.clone().with(Token::Leader(T::LEADER));
                position.reset_source_start();

                if s.match_skip(" ") {
                    position.source.location.start += ' '.len_utf8();
                }
                position.remaining = s;
                *set_line_start = true;
                return Some(Ok(ret));
            } else {
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

impl<'tok, S: StringSlice<'tok>> NextToken<'tok, S> for LineString {
    fn next<'a>(
        this: &mut next_token::Ref<'a, 'tok, S, Self>,
        position: &mut TokenPosition<S>,
    ) -> Option<Result<Source<Token<S>>, Source<Error>>> {
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

impl<'tok, S: StringSlice<'tok>> NextToken<'tok, S> for QuoteString {
    fn next<'a>(
        this: &mut next_token::Ref<'a, 'tok, S, Self>,
        position: &mut TokenPosition<S>,
    ) -> Option<Result<Source<Token<S>>, Source<Error>>> {
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
struct ImmediateResult<S>(Result<Source<Token<S>>, Source<Error>>);

impl<'tok, S: std::fmt::Debug + 'tok> NextToken<'tok, S> for ImmediateResult<S> {
    fn next<'a>(
        this: &mut next_token::Ref<'a, 'tok, S, Self>,
        _position: &mut TokenPosition<S>,
    ) -> Option<Result<Source<Token<S>>, Source<Error>>> {
        Some(this.finish().0)
    }

    fn close(self) -> Result<usize, Source<Error>> {
        panic!("immediate result closing: {:?}", self.0);
    }
}

/// String slice types which can be used in tokenization.
pub trait StringSlice<'a>: Clone + std::fmt::Debug + Sized + 'a {
    fn next_char(&mut self) -> Option<char>;
    fn peek_char(&self) -> Option<char>;
    fn split_at(&self, byte_index: usize) -> (Self, Self);
    fn len(&self) -> usize;
    unsafe fn slice_to(&self, byte_index: usize) -> Self;
    fn from_str(s: &'static str) -> Self;
    fn write_to_string(slices: &[Self], s: &mut String);
    fn ends_with_char(&self, c: char) -> bool;

    fn starts_with_str(&self, s: &str) -> bool {
        let mut slice = self.clone();
        for c in s.chars() {
            if slice.next_char() != Some(c) {
                return false;
            }
        }
        true
    }

    fn starts_with_char(&self, c: char) -> bool {
        self.peek_char() == Some(c)
    }

    fn find<F: FnMut(char) -> bool>(&self, mut f: F) -> Option<usize> {
        let mut slice = self.clone();
        let mut offset = 0;
        while let Some(c) = slice.next_char() {
            if f(c) {
                return Some(offset);
            } else {
                offset += c.len_utf8();
            }
        }
        None
    }

    fn match_skip(&mut self, s: &str) -> bool {
        let ret = self.starts_with_str(s);
        if ret {
            *self = self.split_at(s.len()).1;
        }
        ret
    }

    fn trim_start_matches<F: FnMut(char) -> bool>(&self, mut f: F) -> Self {
        let mut slice = self.clone();
        while let Some(c) = slice.peek_char() {
            if !f(c) {
                break;
            } else {
                slice.next_char();
            }
        }
        slice
    }

    fn is_empty(&self) -> bool {
        self.len() == 0
    }

    fn to_string(slices: &[Self]) -> String {
        let mut s = String::new();
        Self::write_to_string(slices, &mut s);
        s
    }
}

impl<'a> StringSlice<'a> for &'a str {
    fn starts_with_str(&self, s: &str) -> bool {
        self.starts_with(s)
    }

    fn starts_with_char(&self, c: char) -> bool {
        self.starts_with(c)
    }

    fn ends_with_char(&self, c: char) -> bool {
        self.ends_with(c)
    }

    fn len(&self) -> usize {
        str::len(self)
    }

    fn find<F: FnMut(char) -> bool>(&self, f: F) -> Option<usize> {
        str::find(self, f)
    }

    fn split_at(&self, index: usize) -> (Self, Self) {
        str::split_at(self, index)
    }

    fn peek_char(&self) -> Option<char> {
        self.chars().next()
    }

    fn next_char(&mut self) -> Option<char> {
        let mut chars = self.chars();
        let c = chars.next()?;
        *self = chars.as_str();
        Some(c)
    }

    fn match_skip(&mut self, s: &str) -> bool {
        if self.starts_with(s) {
            // SAFETY: `s.len()` must be a valid unicode-boundary offset of self
            *self = unsafe { self.get_unchecked(s.len()..) };
            true
        } else {
            false
        }
    }

    unsafe fn slice_to(&self, byte_index: usize) -> Self {
        self.get_unchecked(..byte_index)
    }

    fn trim_start_matches<F: FnMut(char) -> bool>(&self, f: F) -> Self {
        str::trim_start_matches(self, f)
    }

    fn from_str(s: &'static str) -> Self {
        s
    }

    fn write_to_string(slices: &[Self], s: &mut String) {
        let total = slices.iter().map(|s| s.len()).sum();
        s.reserve(total);
        for slice in slices {
            s.push_str(slice);
        }
    }
}

#[derive(Debug)]
struct TokenPosition<S> {
    remaining: S,
    source: Source<()>,
    line_start: bool,
    next_line_start: bool,
}

/// Iterator producing tokens or errors.
pub struct Tokens<'tok, S> {
    position: TokenPosition<S>,
    next: Next<'tok, S>,
}

impl<'tok, S: StringSlice<'tok>> From<Source<S>> for Tokens<'tok, S> {
    fn from(s: Source<S>) -> Self {
        let (source, remaining) = s.take();
        let mut position = TokenPosition {
            remaining,
            source,
            line_start: true,
            next_line_start: false,
        };
        let mut next = Next::new(Normal::new(FinishAtNone));

        // Check for shebang at beginning of script and handle it specially.
        if position.remaining.starts_with_str("#!") {
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

impl<'tok, S: StringSlice<'tok>> TokenPosition<S> {
    pub fn next_char(&mut self) -> Option<char> {
        if self.next_line_start {
            self.next_line_start = false;
            self.line_start = true;
            return None;
        }
        let c = self.remaining.next_char()?;
        self.source.location.length += c.len_utf8();
        self.next_line_start = c == '\n';
        self.line_start = false;
        Some(c)
    }

    pub fn peek(&self) -> Option<char> {
        self.remaining.peek_char()
    }

    pub fn match_skip(&mut self, s: &str) -> bool {
        if self.remaining.match_skip(s) {
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

    pub fn source<V>(&self, v: V) -> Source<V> {
        self.source.clone().with(v)
    }
}

impl<'tok, S: StringSlice<'tok>> Iterator for Tokens<'tok, S> {
    type Item = Result<Source<Token<S>>, Source<Error>>;

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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn symbols() {
        assert_tokens(
            "=^:!->|<||>#a##a",
            &[
                Token::equal(),
                Token::caret(),
                Token::colon(),
                Token::bang(),
                Token::arrow(),
                Token::pipe(),
                Token::pipe_left(),
                Token::pipe_right(),
                Token::hash(),
                Token::string("a"),
                Token::double_hash(),
                Token::string("a"),
            ],
        );
    }

    #[test]
    fn colon_prefix() {
        assert_tokens(":a", &[Token::colon_prefix(), Token::string("a")]);
        assert_tokens(" :a", &[Token::colon_prefix(), Token::string("a")]);
        assert_tokens(
            "a :b",
            &[
                Token::string("a"),
                Token::colon_prefix(),
                Token::string("b"),
            ],
        );
        assert_tokens(
            "-> :b",
            &[Token::arrow(), Token::colon_prefix(), Token::string("b")],
        );
    }

    #[test]
    fn colon() {
        assert_tokens(
            "a:b",
            &[Token::string("a"), Token::colon(), Token::string("b")],
        );
        assert_err("a:", Error::InvalidColon);
        assert_err("a: ", Error::InvalidColon);
        assert_err(":", Error::InvalidColon);
        assert_err(": ", Error::InvalidColon);
    }

    #[test]
    fn pipe_colon() {
        assert_tokens(
            "\"hi\" |>:something",
            &[
                Token::quote(),
                Token::string("hi"),
                Token::close(),
                Token::pipe_right(),
                Token::colon(),
                Token::string("something"),
            ],
        );
    }

    #[test]
    fn parens() {
        assert_tokens("()", &[Token::paren(), Token::close()]);
    }

    #[test]
    fn parens_separators() {
        assert_err("(,)", Error::InvalidExpressionSeparator);
        assert_err("(;)", Error::InvalidExpressionSeparator);
    }

    #[test]
    fn parens_unclosed() {
        assert_err("(hi", Error::UnmatchedOpeningToken(PairedToken::Paren));
        assert_err(
            "hi)",
            Error::UnmatchedClosingToken(PairedToken::Paren, None),
        );
    }

    #[test]
    fn parens_newlines() {
        assert_tokens("(\n\n\n)", &[Token::paren(), Token::close()]);
    }

    #[test]
    fn brackets() {
        assert_tokens("[]", &[Token::bracket(), Token::close()]);
    }

    #[test]
    fn brackets_separators() {
        assert_tokens("[,]", &[Token::bracket(), Token::next(), Token::close()]);
        assert_tokens("[;]", &[Token::bracket(), Token::next(), Token::close()]);
        assert_tokens("[\n]", &[Token::bracket(), Token::next(), Token::close()]);
    }

    #[test]
    fn brackets_unclosed() {
        assert_err("[hi", Error::UnmatchedOpeningToken(PairedToken::Bracket));
        assert_err(
            "hi]",
            Error::UnmatchedClosingToken(PairedToken::Bracket, None),
        );
    }

    #[test]
    fn curly() {
        assert_tokens("{}", &[Token::curly(), Token::close()]);
    }

    #[test]
    fn curly_separators() {
        assert_tokens("{,}", &[Token::curly(), Token::next(), Token::close()]);
        assert_tokens("{;}", &[Token::curly(), Token::next(), Token::close()]);
        assert_tokens("{\n}", &[Token::curly(), Token::next(), Token::close()]);
    }

    #[test]
    fn curly_unclosed() {
        assert_err("{hi", Error::UnmatchedOpeningToken(PairedToken::Curly));
        assert_err(
            "hi}",
            Error::UnmatchedClosingToken(PairedToken::Curly, None),
        );
    }

    #[test]
    fn top_level_separators() {
        assert_tokens(",;\n", &[Token::next(), Token::next(), Token::next()]);
    }

    #[test]
    fn strings() {
        assert_tokens(
            "hello world",
            &[Token::string("hello"), Token::string("world")],
        );
    }

    #[test]
    fn string_ends() {
        assert_tokens(
            "a[b]c{d}e(f)g:h,i;j^k=l\nm n|o<|p|>q!r->s\"t\"",
            &[
                Token::string("a"),
                Token::bracket(),
                Token::string("b"),
                Token::close(),
                Token::string("c"),
                Token::curly(),
                Token::string("d"),
                Token::close(),
                Token::string("e"),
                Token::paren(),
                Token::string("f"),
                Token::close(),
                Token::string("g"),
                Token::colon(),
                Token::string("h"),
                Token::next(),
                Token::string("i"),
                Token::next(),
                Token::string("j"),
                Token::caret(),
                Token::string("k"),
                Token::equal(),
                Token::string("l"),
                Token::next(),
                Token::string("m"),
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
                Token::string("s"),
                Token::quote(),
                Token::string("t"),
                Token::close(),
            ],
        );
    }

    #[test]
    fn quotes() {
        assert_tokens(
            r#""hello world""#,
            &[Token::quote(), Token::string("hello world"), Token::close()],
        );
    }

    #[test]
    fn quotes_newline() {
        assert_tokens(
            "\"hello\nworld\"",
            &[
                Token::quote(),
                Token::string("hello\n"),
                Token::string("world"),
                Token::close(),
            ],
        );
    }

    #[test]
    fn quotes_nested() {
        assert_tokens(
            "\"^name\"",
            &[
                Token::quote(),
                Token::caret(),
                Token::string("name"),
                Token::close(),
            ],
        );
    }

    #[test]
    fn quotes_unclosed() {
        assert_err("\"hi!", Error::UnmatchedOpeningToken(PairedToken::Quote));
    }

    #[test]
    fn string_nested() {
        assert_tokens(
            "\"hi ^name\"",
            &[
                Token::quote(),
                Token::string("hi "),
                Token::caret(),
                Token::string("name"),
                Token::close(),
            ],
        );
        assert_tokens(
            "\"hi ^(name)\"",
            &[
                Token::quote(),
                Token::string("hi "),
                Token::caret(),
                Token::paren(),
                Token::string("name"),
                Token::close(),
                Token::close(),
            ],
        );
        assert_err("\"hi ^ \"", Error::InvalidStringCaret);
    }

    #[test]
    fn string_nested_escape() {
        assert_tokens(
            "\"hi ^^\"",
            &[
                Token::quote(),
                Token::string("hi "),
                Token::string("^"),
                Token::close(),
            ],
        );
    }

    #[test]
    fn block_string() {
        assert_tokens(
            "' hello\n' world",
            &[
                Token::apostrophe_space(),
                Token::string("hello\n"),
                Token::leading_apostrophe(),
                Token::string("world"),
                Token::close(),
            ],
        );
        assert_tokens(
            "'\n  ' hello\n  '\n  '  world\nhi",
            &[
                Token::apostrophe_space(),
                Token::string("\n"),
                Token::leading_apostrophe(),
                Token::string("hello\n"),
                Token::leading_apostrophe(),
                Token::string("\n"),
                Token::leading_apostrophe(),
                Token::string(" world\n"),
                Token::close(),
                Token::next(),
                Token::string("hi"),
            ],
        );
    }

    #[test]
    fn block_string_no_space() {
        assert_err("'hi", Error::InvalidBlockString);
    }

    #[test]
    fn block_string_nested() {
        assert_tokens(
            "' hello ^name",
            &[
                Token::apostrophe_space(),
                Token::string("hello "),
                Token::caret(),
                Token::string("name"),
                Token::close(),
            ],
        );
    }

    #[test]
    fn hash() {
        assert_tokens(
            "#(a#)",
            &[
                Token::hash(),
                Token::paren(),
                Token::string("a"),
                Token::hash(),
                Token::close(),
            ],
        );
    }

    #[test]
    fn hash_block() {
        assert_tokens(
            "# hello\n# world",
            &[
                Token::hash_space(),
                Token::string("hello\n"),
                Token::leading_hash(),
                Token::string("world"),
                Token::close(),
            ],
        );
        assert_tokens(
            "#\n  # hello\n  #\n  #  world\nhi",
            &[
                Token::hash_space(),
                Token::string("\n"),
                Token::leading_hash(),
                Token::string("hello\n"),
                Token::leading_hash(),
                Token::string("\n"),
                Token::leading_hash(),
                Token::string(" world\n"),
                Token::close(),
                Token::next(),
                Token::string("hi"),
            ],
        );
    }

    #[test]
    fn hash_block_no_nested() {
        assert_tokens(
            "# hello ^name",
            &[
                Token::hash_space(),
                Token::string("hello ^name"),
                Token::close(),
            ],
        );
    }

    #[test]
    fn double_hash() {
        assert_tokens(
            "##(a##)",
            &[
                Token::double_hash(),
                Token::paren(),
                Token::string("a"),
                Token::double_hash(),
                Token::close(),
            ],
        );
    }

    #[test]
    fn double_hash_block() {
        assert_tokens(
            "## hello\n## world",
            &[
                Token::double_hash_space(),
                Token::string("hello\n"),
                Token::leading_double_hash(),
                Token::string("world"),
                Token::close(),
            ],
        );
        assert_tokens(
            "##\n  ## hello\n  ##\n  ##  world\nhi",
            &[
                Token::double_hash_space(),
                Token::string("\n"),
                Token::leading_double_hash(),
                Token::string("hello\n"),
                Token::leading_double_hash(),
                Token::string("\n"),
                Token::leading_double_hash(),
                Token::string(" world\n"),
                Token::close(),
                Token::next(),
                Token::string("hi"),
            ],
        );
    }

    #[test]
    fn double_hash_block_nested() {
        assert_tokens(
            "## hello ^name",
            &[
                Token::double_hash_space(),
                Token::string("hello "),
                Token::caret(),
                Token::string("name"),
                Token::close(),
            ],
        );
    }

    #[test]
    fn mixed_hash_blocks() {
        assert_tokens(
            "# foo\n# bar\n## foo\n## bar\n# foo",
            &[
                Token::hash_space(),
                Token::string("foo\n"),
                Token::leading_hash(),
                Token::string("bar\n"),
                Token::close(),
                Token::next(),
                Token::double_hash_space(),
                Token::string("foo\n"),
                Token::leading_double_hash(),
                Token::string("bar\n"),
                Token::close(),
                Token::next(),
                Token::hash_space(),
                Token::string("foo"),
                Token::close(),
            ],
        )
    }

    #[test]
    fn children() {
        assert_tokens(
            "a b c
        ->;
        b = !, c; d",
            &[
                Token::string("a"),
                Token::string("b"),
                Token::string("c"),
                Token::next(),
                Token::arrow(),
                Token::next(),
                Token::next(),
                Token::string("b"),
                Token::equal(),
                Token::bang(),
                Token::next(),
                Token::string("c"),
                Token::next(),
                Token::string("d"),
            ],
        );
    }

    #[test]
    fn nested() {
        assert_tokens(
            "a (a b\nc d) {a b\nc d} [a b\nc d] ({a b} c [d e\nf {g (h)}])\n(i)",
            &[
                Token::string("a"),
                Token::paren(),
                Token::string("a"),
                Token::string("b"),
                Token::string("c"),
                Token::string("d"),
                Token::close(),
                Token::curly(),
                Token::string("a"),
                Token::string("b"),
                Token::next(),
                Token::string("c"),
                Token::string("d"),
                Token::close(),
                Token::bracket(),
                Token::string("a"),
                Token::string("b"),
                Token::next(),
                Token::string("c"),
                Token::string("d"),
                Token::close(),
                Token::paren(),
                Token::curly(),
                Token::string("a"),
                Token::string("b"),
                Token::close(),
                Token::string("c"),
                Token::bracket(),
                Token::string("d"),
                Token::string("e"),
                Token::next(),
                Token::string("f"),
                Token::curly(),
                Token::string("g"),
                Token::paren(),
                Token::string("h"),
                Token::close(),
                Token::close(),
                Token::close(),
                Token::close(),
                Token::next(),
                Token::paren(),
                Token::string("i"),
                Token::close(),
            ],
        )
    }

    #[test]
    fn nested_blocks() {
        assert_tokens(
            "## a b ^{
             ##  v = 5
             ##
             ##  x = '
             ##  ' this is
             ##  ' nested ^v
             ## }",
            &[
                Token::double_hash_space(),
                Token::string("a b "),
                Token::caret(),
                Token::curly(),
                Token::next(),
                Token::leading_double_hash(),
                Token::string("v"),
                Token::equal(),
                Token::string("5"),
                Token::next(),
                Token::leading_double_hash(),
                Token::next(),
                Token::leading_double_hash(),
                Token::string("x"),
                Token::equal(),
                Token::apostrophe_space(),
                Token::string("\n"),
                Token::leading_double_hash(),
                Token::leading_apostrophe(),
                Token::string("this is\n"),
                Token::leading_double_hash(),
                Token::leading_apostrophe(),
                Token::string("nested "),
                Token::caret(),
                Token::string("v"),
                Token::string("\n"),
                Token::leading_double_hash(),
                Token::close(),
                Token::next(),
                Token::close(),
                Token::close(),
            ],
        );
    }

    #[test]
    fn nested_mismatch() {
        assert_err(
            "(a {b c\n d [e,f})",
            Error::UnmatchedClosingToken(
                PairedToken::Curly,
                Some(Source::missing(PairedToken::Bracket)),
            ),
        );
        assert_err(
            "a {b c\n d [e,f]})",
            Error::UnmatchedClosingToken(PairedToken::Paren, None),
        );
        assert_err("## ^(\na", Error::UnmatchedOpeningToken(PairedToken::Paren));
    }

    #[test]
    fn shebang() {
        assert_tokens(
            "#!/usr/bin/env ergo\na b c",
            &[
                Token::hash(),
                Token::bang(),
                Token::string("/usr/bin/env ergo\n"),
                Token::string("a"),
                Token::string("b"),
                Token::string("c"),
            ],
        );
    }

    fn assert_tokens(s: &str, expected: &[Token<&str>]) {
        let toks: Vec<_> = Tokens::from(Source::missing(s))
            .collect::<Result<Vec<_>, _>>()
            .expect("failed to tokenize")
            .into_iter()
            .map(|t| t.unwrap())
            .collect();
        dbg!(&toks);
        assert!(toks == expected);
    }

    fn assert_err(s: &str, err: Error) {
        let e = Tokens::from(Source::missing(s))
            .collect::<Result<Vec<_>, _>>()
            .expect_err("tokenization should have errored");
        dbg!(&e);
        assert!(e == err);
    }
}
