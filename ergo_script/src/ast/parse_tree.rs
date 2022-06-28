//! Tree parsing.
//!
//! A basic syntax tree is formed, and in the process:
//! * Pipe operators are desugared.
//! * Redundant groupings (parens) are removed.
//! * Quoted string escape sequences are interpreted.

use super::tokenize::{PairedToken, StringSlice, SymbolicToken, Token};
use ergo_runtime::{source::IntoSource, Source};
use std::fmt;

pub type TreeVec = Vec<Source<Tree>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Tree {
    String(String),
    Caret(Box<Source<Tree>>),
    ColonPrefix(Box<Source<Tree>>),
    Dollar(Box<Source<Tree>>),
    DollarQuestion(Box<Source<Tree>>),
    Hash(Box<Source<Tree>>),
    DoubleHash(Box<Source<Tree>>),
    Equal(Box<Source<Tree>>, Box<Source<Tree>>),
    Tilde(Box<Source<Tree>>),
    TildeEqual(Box<Source<Tree>>, Box<Source<Tree>>),
    Arrow(Box<Source<Tree>>, Box<Source<Tree>>),
    Colon(Box<Source<Tree>>, Box<Source<Tree>>),
    // Groups
    Parens(TreeVec),
    Curly(TreeVec),
    Bracket(TreeVec),
    // String Groups
    Quote(StringTreeVec),
    ApostropheBlock(StringTreeVec),
    HashBlock(String),
    DoubleHashBlock(StringTreeVec),
}

pub type StringTreeVec = Vec<Source<StringTree>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StringTree {
    String(String),
    Expression(Source<Tree>),
}

/// A tree parser.
///
/// This is used as an iterator, producing Source<Tree> as individual items in a script.
pub struct Parser<TokenIter: Iterator> {
    tokens: std::iter::Peekable<TokenIter>,
}

/// Tree parsing errors.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error<TokError> {
    /// A tokenization error.
    Tokenization(TokError),
    #[allow(dead_code)]
    /// A unary operator is missing its argument (which should be before it).
    UnaryMissingPrefix,
    /// A unary operator is missing its argument (which should be after it).
    UnaryMissingSuffix,
    /// A binary operator is missing its previous argument.
    BinaryMissingPrevious,
    /// A binary operator is missing its next argument.
    BinaryMissingNext,
    /// An escape sequence in a quoted string was not recognized.
    UnrecognizedEscapeSequence,
    /// A unicode character escape sequence is missing a closing character.
    UnfinishedUnicodeCharacter,
    /// A unicode character escape sequence contained an invalid digit.
    InvalidUnicodeCharacter,
}

impl<E: fmt::Display> fmt::Display for Error<E> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Tokenization(t) => t.fmt(f),
            Error::UnaryMissingPrefix => write!(f, "expected an argument before the operator"),
            Error::UnaryMissingSuffix => write!(f, "expected an argument after the operator"),
            Error::BinaryMissingPrevious => write!(f, "expected an argument before the operator"),
            Error::BinaryMissingNext => write!(f, "expected an argument after the operator"),
            Error::UnrecognizedEscapeSequence => write!(f, "unrecognized escape sequence"),
            Error::UnfinishedUnicodeCharacter => write!(
                f,
                "unfinished unicode character code (missing a closing `}}`)"
            ),
            Error::InvalidUnicodeCharacter => write!(
                f,
                "invalid unicode character code (1-6 hex digits required)"
            ),
        }
    }
}

impl<E: std::error::Error + 'static> std::error::Error for Error<E> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::Tokenization(t) => Some(t),
            _ => None,
        }
    }
}

impl<E: super::ToDiagnostic + fmt::Display> super::ToDiagnostic for Error<E> {
    fn additional_info(&self, diagnostic: &mut ergo_runtime::error::Diagnostic) {
        match self {
            Error::Tokenization(e) => e.additional_info(diagnostic),
            _ => (),
        }
    }
}

struct Errors<TokError>(Vec<Source<Error<TokError>>>);

impl<E> From<Source<Error<E>>> for Errors<E> {
    fn from(e: Source<Error<E>>) -> Self {
        Errors(vec![e])
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum TreeOrSymbol {
    Tree(Tree),
    Symbol(SymbolicToken),
    Used,
}

impl From<Tree> for TreeOrSymbol {
    fn from(t: Tree) -> Self {
        TreeOrSymbol::Tree(t)
    }
}

impl From<SymbolicToken> for TreeOrSymbol {
    fn from(s: SymbolicToken) -> Self {
        TreeOrSymbol::Symbol(s)
    }
}

enum Single<T> {
    None,
    One(T),
    Many,
}

impl<T> std::ops::Add for Single<T> {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        use Single::*;
        match (self, rhs) {
            (None, None) => None,
            (One(v), None) => One(v),
            (None, One(v)) => One(v),
            _ => Many,
        }
    }
}

impl<T> From<Single<T>> for Option<T> {
    fn from(s: Single<T>) -> Self {
        match s {
            Single::One(t) => Some(t),
            _ => None,
        }
    }
}

impl<T> From<Option<T>> for Single<T> {
    fn from(o: Option<T>) -> Self {
        match o {
            Some(t) => Single::One(t),
            None => Single::None,
        }
    }
}

impl<T> std::iter::Sum for Single<T> {
    fn sum<I>(iter: I) -> Self
    where
        I: Iterator<Item = Self>,
    {
        iter.reduce(|a, b| a + b).unwrap_or(Single::None)
    }
}

impl Tree {
    pub fn is_string(&self) -> bool {
        self.string().is_some()
    }

    pub fn string(&self) -> Option<&str> {
        match self {
            Tree::String(s) => Some(s),
            _ => None,
        }
    }

    /// If the tree contains exactly one ColonPrefix(String) element, return the string.
    pub fn single_colon_string(&self) -> Option<&str> {
        self.single_colon_string_impl().into()
    }

    fn single_colon_string_impl(&self) -> Single<&str> {
        use Tree::*;

        match self {
            String(_) => Single::None,
            Caret(t) | Dollar(t) | DollarQuestion(t) | Hash(t) | DoubleHash(t) | Tilde(t) => {
                t.single_colon_string_impl()
            }
            ColonPrefix(t) => t.string().into(),
            Equal(a, b) | TildeEqual(a, b) | Arrow(a, b) | Colon(a, b) => {
                a.single_colon_string_impl() + b.single_colon_string_impl()
            }
            Parens(tv) | Curly(tv) | Bracket(tv) => {
                tv.iter().map(|t| t.single_colon_string_impl()).sum()
            }
            Quote(stv) | ApostropheBlock(stv) | DoubleHashBlock(stv) => stv
                .iter()
                .map(|st| match st.value() {
                    StringTree::String(_) => Single::None,
                    StringTree::Expression(t) => t.single_colon_string_impl(),
                })
                .sum(),
            HashBlock(_) => Single::None,
        }
    }
}

impl TreeOrSymbol {
    pub fn take(&mut self) -> Self {
        std::mem::replace(self, TreeOrSymbol::Used)
    }

    pub fn is_symbol(&self, s: &SymbolicToken) -> bool {
        match self {
            TreeOrSymbol::Symbol(t) => t == s,
            _ => false,
        }
    }

    pub fn is_tree(&self) -> bool {
        match self {
            TreeOrSymbol::Tree(_) => true,
            _ => false,
        }
    }

    pub fn as_tree(self) -> Tree {
        match self {
            TreeOrSymbol::Tree(t) => t,
            _ => panic!("not a tree"),
        }
    }

    pub fn is_leftward_pipe(&self) -> bool {
        self.is_symbol(&SymbolicToken::PipeLeft)
    }

    pub fn is_rightward_pipe(&self) -> bool {
        self.is_symbol(&SymbolicToken::Pipe) || self.is_symbol(&SymbolicToken::PipeRight)
    }
}

impl<T: IntoIterator> From<T> for Parser<T::IntoIter> {
    fn from(iter: T) -> Self {
        Parser {
            tokens: iter.into_iter().peekable(),
        }
    }
}

impl<'a, T, I, E: fmt::Debug> Iterator for Parser<I>
where
    I: Iterator<Item = Result<Source<Token<T>>, Source<E>>>,
    T: StringSlice<'a> + Into<String> + PartialEq,
{
    type Item = Result<Source<Tree>, Vec<Source<Error<E>>>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.tokens.peek().is_none() {
            None
        } else {
            match self.group_as_tree() {
                Ok(Some(v)) => Some(Ok(v)),
                Ok(None) => None,
                Err(errs) => Some(Err(errs.0)),
            }
        }
    }
}

fn parse_escape_sequences<'a, T: StringSlice<'a>, E>(
    mut s: T,
) -> Result<String, (Error<E>, usize, usize)> {
    let mut ret = String::with_capacity(s.len());
    let mut escape = None;

    let string_end = s.len();

    let mut offset = 0;
    while let Some(c) = s.next_char() {
        match escape.take() {
            None => {
                if c == '\\' {
                    escape = Some(offset);
                } else {
                    ret.push(c);
                }
            }
            Some(start_offset) => {
                let end_offset = offset + c.len_utf8();
                match c {
                    '"' | '\\' => ret.push(c),
                    'n' => ret.push('\n'),
                    't' => ret.push('\t'),
                    'u' => {
                        if let Some('{') = s.next_char() {
                            let unicode_hex = s.clone();
                            let mut unicode_hex_len = 0;
                            let mut end = false;
                            while let Some(c) = s.next_char() {
                                if c == '}' {
                                    end = true;
                                    break;
                                } else {
                                    unicode_hex_len += c.len_utf8();
                                }
                            }
                            if !end {
                                return Err((
                                    Error::UnfinishedUnicodeCharacter,
                                    start_offset,
                                    string_end,
                                ));
                            } else {
                                // SAFETY: end must be in bounds and on a character boundary
                                // according to the character iterator.
                                let unicode_hex = unsafe { unicode_hex.slice_to(unicode_hex_len) };
                                let end_offset =
                                    end_offset + '{'.len_utf8() + unicode_hex_len + '}'.len_utf8();
                                // TODO could use a Cow<'str> method instead
                                let unicode_hex = T::to_string(&[unicode_hex]);
                                ret.push(
                                    u32::from_str_radix(&unicode_hex, 16)
                                        .ok()
                                        .and_then(|val| char::try_from(val).ok())
                                        .ok_or((
                                            Error::InvalidUnicodeCharacter,
                                            start_offset,
                                            end_offset,
                                        ))?,
                                );
                            }
                        } else {
                            return Err((
                                Error::UnrecognizedEscapeSequence,
                                start_offset,
                                end_offset,
                            ));
                        }
                    }
                    _ => return Err((Error::UnrecognizedEscapeSequence, start_offset, end_offset)),
                }
            }
        }
        offset += c.len_utf8();
    }

    Ok(ret)
}

impl<'a, T, I, E: fmt::Debug> Parser<I>
where
    I: Iterator<Item = Result<Source<Token<T>>, Source<E>>>,
    T: StringSlice<'a> + Into<String> + PartialEq,
{
    fn next_tok(&mut self) -> Result<Option<Source<Token<T>>>, Source<Error<E>>> {
        match self.tokens.next() {
            None => Ok(None),
            Some(Err(e)) => Err(e.map(Error::Tokenization)),
            Some(Ok(t)) => {
                // Always disregard leader tokens.
                if let Token::Leader(_) = t.value() {
                    self.next_tok()
                } else {
                    Ok(Some(t))
                }
            }
        }
    }

    fn tree_or_symbol(&mut self) -> Result<Option<Source<TreeOrSymbol>>, Errors<E>> {
        match self.next_tok()? {
            None => Ok(None),
            Some(t) => {
                let (source, t) = t.take();
                Ok(Some(match t {
                    Token::Symbol(s) => source.with(s.into()),
                    Token::String(s) => source.with(TreeOrSymbol::Tree(Tree::String(s.into()))),
                    Token::StartNested(p) => {
                        let mut src = None;
                        let t = match p {
                            PairedToken::Paren => {
                                let grp = self.group()?;
                                if grp.len() == 1 {
                                    let (inner_src, inner) = grp.into_iter().next().unwrap().take();
                                    src = Some(inner_src);
                                    inner
                                } else {
                                    Tree::Parens(grp)
                                }
                            }
                            PairedToken::Curly => Tree::Curly(self.groups()?),
                            PairedToken::Bracket => Tree::Bracket(self.groups()?),
                            PairedToken::Quote => Tree::Quote(self.string_tree(true)?),
                            PairedToken::Apostrophe => {
                                Tree::ApostropheBlock(self.string_tree(false)?)
                            }
                            PairedToken::Hash => Tree::HashBlock(self.join_strings()?),
                            PairedToken::DoubleHash => {
                                Tree::DoubleHashBlock(self.string_tree(false)?)
                            }
                        };
                        // Next token must be valid and EndNested
                        let end = self.next_tok().unwrap().unwrap();
                        debug_assert!(*end == Token::EndNested);
                        src.unwrap_or_else(|| (source, end).into_source().source())
                            .with(t.into())
                    }
                    _ => panic!("unexpected token"),
                }))
            }
        }
    }

    fn group(&mut self) -> Result<TreeVec, Errors<E>> {
        let mut parts: Vec<Source<TreeOrSymbol>> = Default::default();

        // Consume extra NextChild without any content
        while self
            .tokens
            .peek()
            .map(|t| t.as_ref().map(|t| *t == Token::NextChild).unwrap_or(false))
            .unwrap_or(false)
        {
            self.tokens.next();
        }

        // Read all tokens until end of current group or next child separator.
        while self
            .tokens
            .peek()
            .map(|t| {
                t.as_ref()
                    .map(|t| *t != Token::EndNested && *t != Token::NextChild)
                    .unwrap_or(true)
            })
            .unwrap_or(false)
        {
            parts.push(self.tree_or_symbol()?.unwrap());
        }

        if parts.is_empty() {
            return Ok(vec![]);
        }

        // Resolve operators
        // Precedence (descending):
        //   Dollar/DollarQuestion Prefix
        //   Colon Prefix
        //   Colon (Left assoc)
        //   TildeEqual (Left assoc)
        //   Tilde Prefix
        //   Caret Prefix
        //   Hash/DoubleHash Prefix
        //   Arrow (Right assoc)
        //   Pipe/PipeRight (Left assoc macro)
        //   PipeLeft (Right assoc macro)
        //   Equal (Left assoc)
        //   Caret Expression Prefix

        type Toks = [Source<TreeOrSymbol>];
        type TreeDeque = std::collections::VecDeque<Source<Tree>>;
        type TResult<E> = Result<TreeDeque, Source<Error<E>>>;

        fn to_tree(td: TreeDeque) -> Source<Tree> {
            if td.len() == 1 {
                td.into_iter().next().unwrap()
            } else {
                td.into_source()
                    .map(|v| Tree::Parens(v.into_iter().collect()))
            }
        }

        fn single(t: Source<Tree>) -> TreeDeque {
            vec![t].into()
        }

        fn prefixed_initial<Inner, Create, E>(
            toks: &mut Toks,
            sym: &SymbolicToken,
            inner: Inner,
            create: Create,
        ) -> TResult<E>
        where
            Inner: FnOnce(&mut Toks) -> TResult<E>,
            Create: FnOnce(Box<Source<Tree>>) -> Tree + Clone,
        {
            let is_sym = if let Some(s) = toks.first() {
                s.is_symbol(sym)
            } else {
                false
            };
            if is_sym {
                let (t, rest) = toks.split_at_mut(1);
                let t = unsafe { t.get_unchecked_mut(0) }.as_mut().map(|t| t.take());
                let result = prefixed_initial(rest, sym, inner, create.clone())?;
                if result.is_empty() {
                    Err(t.with(Error::UnaryMissingSuffix))
                } else {
                    Ok(single(
                        (t, to_tree(result))
                            .into_source()
                            .map(|(_, g)| create(g.into())),
                    ))
                }
            } else {
                inner(toks)
            }
        }

        // TODO use split_inclusive_mut when available to avoid recursion in ops

        fn left_bin_op<Pred, Part, Join, E>(
            toks: &mut Toks,
            syms: Pred,
            part: Part,
            join: Join,
        ) -> TResult<E>
        where
            Pred: Fn(&TreeOrSymbol) -> bool,
            Part: Fn(&mut Toks) -> TResult<E>,
            Join: Fn(TreeOrSymbol, TreeDeque, TreeDeque) -> TreeDeque + Clone,
        {
            let split = toks.iter().rposition(|t| syms(&*t));
            if let Some(pos) = split {
                let (a, b) = toks.split_at_mut(pos);
                let (t, b) = b.split_at_mut(1);
                let t = unsafe { t.get_unchecked_mut(0) }.as_mut().map(|t| t.take());

                if a.is_empty() {
                    return Err(t.with(Error::BinaryMissingPrevious));
                } else if b.is_empty() {
                    return Err(t.with(Error::BinaryMissingNext));
                }

                let t = t.unwrap();

                let b = part(b)?;
                Ok(join(t, left_bin_op(a, syms, part, join.clone())?, b))
            } else {
                part(toks)
            }
        }

        fn right_bin_op<Pred, Part, Join, E>(
            toks: &mut Toks,
            syms: Pred,
            part: Part,
            join: Join,
        ) -> TResult<E>
        where
            Pred: Fn(&TreeOrSymbol) -> bool,
            Part: Fn(&mut Toks) -> TResult<E>,
            Join: Fn(TreeOrSymbol, TreeDeque, TreeDeque) -> TreeDeque + Clone,
        {
            // TODO use split_inclusive_mut when available, avoid recursion
            let split = toks.iter().position(|t| syms(&*t));
            if let Some(pos) = split {
                let (a, b) = toks.split_at_mut(pos);
                let (t, b) = b.split_at_mut(1);
                let t = unsafe { t.get_unchecked_mut(0) }.as_mut().map(|t| t.take());

                if a.is_empty() {
                    return Err(t.with(Error::BinaryMissingPrevious));
                } else if b.is_empty() {
                    return Err(t.with(Error::BinaryMissingNext));
                }

                let t = t.unwrap();

                let a = part(a)?;
                Ok(join(t, a, right_bin_op(b, syms, part, join.clone())?))
            } else {
                part(toks)
            }
        }

        fn prefix_op<Pred, Part, Join, E>(
            toks: &mut Toks,
            syms: Pred,
            part: Part,
            join: Join,
        ) -> TResult<E>
        where
            Pred: Fn(&TreeOrSymbol) -> bool,
            Part: Fn(&mut Toks) -> TResult<E>,
            Join: Fn(TreeOrSymbol, Source<Tree>) -> Tree + Clone,
        {
            let split = toks.iter().position(|p| syms(&*p));
            if let Some(pos) = split {
                let (a, b) = toks.split_at_mut(pos);
                let (t, b) = b.split_at_mut(1);
                let t = unsafe { t.get_unchecked_mut(0) }.as_mut().map(|t| t.take());

                if b.is_empty() {
                    return Err(t.with(Error::UnaryMissingSuffix));
                }

                let a = part(a)?;
                let mut td = prefix_op(b, syms, part, join.clone())?;
                let new_t = (t, td.pop_front().unwrap())
                    .into_source()
                    .map(|(t, v)| join(t.unwrap(), v));
                let mut ret = a;
                ret.push_back(new_t);
                ret.extend(td);
                Ok(ret)
            } else {
                part(toks)
            }
        }

        #[allow(dead_code)]
        fn suffix_op<Pred, Part, Join, E>(
            toks: &mut Toks,
            syms: Pred,
            part: Part,
            join: Join,
        ) -> TResult<E>
        where
            Pred: Fn(&TreeOrSymbol) -> bool,
            Part: Fn(&mut Toks) -> TResult<E>,
            Join: Fn(TreeOrSymbol, Source<Tree>) -> Tree + Clone,
        {
            let split = toks.iter().rposition(|p| syms(&*p));
            if let Some(pos) = split {
                let (a, b) = toks.split_at_mut(pos);
                let (t, b) = b.split_at_mut(1);
                let t = unsafe { t.get_unchecked_mut(0) }.as_mut().map(|t| t.take());

                if a.is_empty() {
                    return Err(t.with(Error::UnaryMissingPrefix));
                }

                let b = part(b)?;
                let mut td = suffix_op(a, syms, part, join.clone())?;
                let new_t = (td.pop_back().unwrap(), t)
                    .into_source()
                    .map(|(v, t)| join(t.unwrap(), v));
                td.push_back(new_t);
                td.extend(b);
                Ok(td)
            } else {
                part(toks)
            }
        }

        fn to_treedeque<E>(toks: &mut Toks) -> TResult<E> {
            Ok(toks
                .iter_mut()
                .map(|t| t.as_mut().map(|t| t.take().as_tree()))
                .collect())
        }

        fn dollar<E>(toks: &mut Toks) -> TResult<E> {
            prefix_op(
                toks,
                |v| {
                    v.is_symbol(&SymbolicToken::Dollar)
                        | v.is_symbol(&SymbolicToken::DollarQuestion)
                },
                to_treedeque,
                |s, t| match s {
                    TreeOrSymbol::Symbol(SymbolicToken::Dollar) => Tree::Dollar(t.into()),
                    TreeOrSymbol::Symbol(SymbolicToken::DollarQuestion) => {
                        Tree::DollarQuestion(t.into())
                    }
                    _ => panic!("unexpected symbol"),
                },
            )
        }

        fn colon_prefix<E>(toks: &mut Toks) -> TResult<E> {
            prefix_op(
                toks,
                |v| v.is_symbol(&SymbolicToken::ColonPrefix),
                dollar,
                |_, t| Tree::ColonPrefix(t.into()),
            )
        }

        fn colon<E>(toks: &mut Toks) -> TResult<E> {
            // Change Colon to ColonPrefix when appropriate (when prior TreeOrSymbol isn't a tree).
            for s in toks
                .split_inclusive_mut(|t| t.value() == &TreeOrSymbol::Symbol(SymbolicToken::Colon))
            {
                if s.len() == 0 {
                    continue;
                }
                if s[s.len() - 1] == TreeOrSymbol::Symbol(SymbolicToken::Colon)
                    && (s.len() == 1 || !s[s.len() - 2].is_tree())
                {
                    *s[s.len() - 1] = TreeOrSymbol::Symbol(SymbolicToken::ColonPrefix);
                }
            }
            left_bin_op(
                toks,
                |v| v.is_symbol(&SymbolicToken::Colon),
                colon_prefix,
                |_, mut a, mut b| {
                    let ac = a.pop_back().unwrap();
                    let mut ret = a;
                    let bc = b.pop_front().unwrap();
                    let c = (ac, bc)
                        .into_source()
                        .map(|(a, b)| Tree::Colon(a.into(), b.into()));
                    ret.push_back(c);
                    ret.extend(b);
                    ret
                },
            )
        }

        fn tilde_equal<E>(toks: &mut Toks) -> TResult<E> {
            left_bin_op(
                toks,
                |v| v.is_symbol(&SymbolicToken::TildeEqual),
                colon,
                |_, mut a, mut b| {
                    let ac = a.pop_back().unwrap();
                    let mut ret = a;
                    let bc = b.pop_front().unwrap();
                    let c = (ac, bc)
                        .into_source()
                        .map(|(a, b)| Tree::TildeEqual(a.into(), b.into()));
                    ret.push_back(c);
                    ret.extend(b);
                    ret
                },
            )
        }

        fn tilde_prefixes<E>(toks: &mut Toks) -> TResult<E> {
            prefix_op(
                toks,
                |t| t.is_symbol(&SymbolicToken::Tilde),
                tilde_equal,
                |s, t| match s {
                    TreeOrSymbol::Symbol(SymbolicToken::Tilde) => Tree::Tilde(t.into()),
                    _ => panic!("unexpected symbol"),
                },
            )
        }

        fn caret_prefixes<E>(toks: &mut Toks) -> TResult<E> {
            prefix_op(
                toks,
                |t| t.is_symbol(&SymbolicToken::Caret),
                tilde_prefixes,
                |s, t| match s {
                    TreeOrSymbol::Symbol(SymbolicToken::Caret) => Tree::Caret(t.into()),
                    _ => panic!("unexpected symbol"),
                },
            )
        }

        fn hash_prefixes<E>(toks: &mut Toks) -> TResult<E> {
            prefix_op(
                toks,
                |t| t.is_symbol(&SymbolicToken::Hash) || t.is_symbol(&SymbolicToken::DoubleHash),
                caret_prefixes,
                |s, t| match s {
                    TreeOrSymbol::Symbol(SymbolicToken::Hash) => Tree::Hash(t.into()),
                    TreeOrSymbol::Symbol(SymbolicToken::DoubleHash) => Tree::DoubleHash(t.into()),
                    _ => panic!("unexpected symbol"),
                },
            )
        }

        fn arrow<E>(toks: &mut Toks) -> TResult<E> {
            right_bin_op(
                toks,
                |v| v.is_symbol(&SymbolicToken::Arrow),
                hash_prefixes,
                |_, a, b| {
                    single((a, b).into_source().map(|(a, b)| {
                        Tree::Arrow(to_tree(a.unwrap()).into(), to_tree(b.unwrap()).into())
                    }))
                },
            )
        }

        fn consume<'a>(toks: &'a mut Toks) -> impl Iterator<Item = Source<TreeOrSymbol>> + 'a {
            toks.iter_mut().map(|v| v.as_mut().map(|v| v.take()))
        }

        fn pipe_right<E>(toks: &mut Toks) -> TResult<E> {
            let mut rewritten = Vec::new();
            let next = arrow;

            let mut to_append: Option<Source<TreeOrSymbol>> = None;
            let mut remaining = toks;
            while let Some(pos) = remaining.iter().position(|t| t.is_rightward_pipe()) {
                let (a, b) = remaining.split_at_mut(pos);
                let (t, b) = b.split_at_mut(1);
                let t = unsafe { t.get_unchecked_mut(0) }
                    .as_mut()
                    .map(|t| t.take())
                    .unwrap();
                remaining = b;
                rewritten.extend(consume(a));
                if let Some(t) = to_append.take() {
                    rewritten.push(t);
                }
                let a_tree = to_tree(next(&mut rewritten)?).map(TreeOrSymbol::Tree);
                rewritten.clear();
                match t {
                    TreeOrSymbol::Symbol(SymbolicToken::Pipe) => {
                        to_append = Some(a_tree);
                    }
                    TreeOrSymbol::Symbol(SymbolicToken::PipeRight) => {
                        rewritten.push(a_tree);
                    }
                    _ => panic!("unexpected token"),
                }
            }

            if !rewritten.is_empty() || to_append.is_some() {
                rewritten.extend(consume(remaining));
                if let Some(t) = to_append.take() {
                    rewritten.push(t);
                }
                remaining = &mut rewritten;
            }
            next(remaining)
        }

        fn pipe_left<E>(toks: &mut Toks) -> TResult<E> {
            let mut rewritten = Vec::new();
            let next = pipe_right;

            let mut to_append: Option<Source<TreeOrSymbol>> = None;
            let mut remaining = toks;
            while let Some(pos) = remaining.iter().rposition(|t| t.is_leftward_pipe()) {
                let (a, b) = remaining.split_at_mut(pos);
                let (t, b) = b.split_at_mut(1);
                let t = unsafe { t.get_unchecked_mut(0) }
                    .as_mut()
                    .map(|t| t.take())
                    .unwrap();
                remaining = a;
                rewritten.extend(consume(b));
                if let Some(t) = to_append.take() {
                    rewritten.push(t);
                }
                let b_tree = to_tree(next(&mut rewritten)?).map(TreeOrSymbol::Tree);
                rewritten.clear();
                match t {
                    TreeOrSymbol::Symbol(SymbolicToken::PipeLeft) => {
                        to_append = Some(b_tree);
                    }
                    _ => panic!("unexpected token"),
                }
            }

            if !rewritten.is_empty() || to_append.is_some() {
                rewritten.extend(consume(remaining));
                if let Some(t) = to_append.take() {
                    rewritten.push(t);
                }
                remaining = &mut rewritten;
            }
            next(remaining)
        }

        fn eq<E>(toks: &mut Toks) -> TResult<E> {
            left_bin_op(
                toks,
                |v| v.is_symbol(&SymbolicToken::Equal),
                pipe_left,
                |_, a, b| {
                    single((a, b).into_source().map(|(a, b)| {
                        Tree::Equal(to_tree(a.unwrap()).into(), to_tree(b.unwrap()).into())
                    }))
                },
            )
        }

        fn caret_prefix<E>(toks: &mut Toks) -> TResult<E> {
            prefixed_initial(toks, &SymbolicToken::Caret, eq, Tree::Caret)
        }

        caret_prefix(&mut parts)
            .map(|v| v.into())
            .map_err(|e| e.into())
    }

    fn group_as_tree(&mut self) -> Result<Option<Source<Tree>>, Errors<E>> {
        let tv = self.group()?;
        if tv.len() == 0 {
            Ok(None)
        } else if tv.len() == 1 {
            Ok(Some(tv.into_iter().next().unwrap()))
        } else {
            Ok(Some(tv.into_source().map(Tree::Parens)))
        }
    }

    fn groups(&mut self) -> Result<TreeVec, Errors<E>> {
        self.until_end_nested(Self::group_as_tree)
    }

    fn next_as_string_tree(
        &mut self,
        quoted: bool,
        remove_leading_newline: bool,
    ) -> Result<Option<Source<StringTree>>, Errors<E>> {
        match self.next_tok()? {
            None => Ok(None),
            Some(t) => {
                let (mut source, t) = t.take();
                match t {
                    Token::Symbol(SymbolicToken::Dollar) => {
                        let tree = self
                            .tree_or_symbol()?
                            .expect("dollar must be followed by a token")
                            .map(|t| t.as_tree());
                        Ok(Some(
                            (source, tree.source())
                                .into_source()
                                .with(StringTree::Expression(tree)),
                        ))
                    }
                    Token::String(mut s) => {
                        if s.starts_with_char('\n') && remove_leading_newline {
                            Ok(None)
                        } else {
                            // Check if next token is the end
                            let remove_trailing_newline = !quoted
                                && self
                                    .tokens
                                    .peek()
                                    .unwrap()
                                    .as_ref()
                                    .map(|t| t == &Token::EndNested)
                                    .unwrap_or(false);
                            if remove_trailing_newline && s.ends_with_char('\n') {
                                // SAFETY: ends_with() guarantees the offset will be valid
                                s = unsafe { s.slice_to(s.len() - '\n'.len_utf8()) };
                                if s.is_empty() {
                                    return Ok(None);
                                }
                            }
                            if quoted {
                                match parse_escape_sequences(s) {
                                    Ok(s) => Ok(Some(source.with(StringTree::String(s)))),
                                    Err((e, start_offset, end_offset)) => {
                                        source.location.start += start_offset;
                                        source.location.length = end_offset - start_offset;
                                        Err(source.with(e).into())
                                    }
                                }
                            } else {
                                Ok(Some(source.with(StringTree::String(s.into()))))
                            }
                        }
                    }
                    _ => panic!("unexpected token"),
                }
            }
        }
    }

    fn string_tree(&mut self, quoted: bool) -> Result<StringTreeVec, Errors<E>> {
        let mut first = true;
        self.until_end_nested(|this| {
            this.next_as_string_tree(quoted, !quoted && std::mem::replace(&mut first, false))
        })
    }

    fn join_strings(&mut self) -> Result<String, Errors<E>> {
        self.until_end_nested(|this| match this.next_tok()? {
            None => Ok(None),
            Some(st) => match st.unwrap() {
                Token::String(s) => Ok(Some(s)),
                _ => panic!("unexpected token"),
            },
        })
        .map(|strs| T::to_string(&strs))
    }

    fn until_end_nested<U, F>(&mut self, mut f: F) -> Result<Vec<U>, Errors<E>>
    where
        F: FnMut(&mut Self) -> Result<Option<U>, Errors<E>>,
    {
        let mut result = Vec::new();
        let mut errors = Vec::new();

        while self
            .tokens
            .peek()
            .map(|t| t.as_ref().map(|t| *t != Token::EndNested).unwrap_or(true))
            .unwrap_or(false)
        {
            match f(self) {
                Ok(Some(v)) => result.push(v),
                Ok(None) => (),
                Err(errs) => errors.extend(errs.0),
            }
        }

        if errors.is_empty() {
            Ok(result)
        } else {
            Err(Errors(errors))
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::tokenize::Tokens;

    use Tree::*;

    fn src<T, R>(e: T) -> R
    where
        R: From<Source<T>>,
    {
        Source::missing(e).into()
    }

    fn s<R>(s: &str) -> R
    where
        R: From<Source<Tree>>,
    {
        src(String(s.into()))
    }

    #[test]
    fn string() {
        assert_single("s", String("s".into()));
    }

    #[test]
    fn caret() {
        assert_single("^a", Caret(s("a")));
        assert_single("^a b c", Caret(src(Parens(vec![s("a"), s("b"), s("c")]))));
        assert_single("a ^b c", Parens(vec![s("a"), src(Caret(s("b"))), s("c")]));
    }

    #[test]
    fn tilde() {
        assert_single("~a", Tilde(s("a")));
        assert_single("~a=b", Tilde(src(TildeEqual(s("a"), s("b")))));
        assert_single("~a = b", Equal(src(Tilde(s("a"))), s("b")));
    }

    #[test]
    fn colon_prefix() {
        assert_single(":a", ColonPrefix(s("a")));
        assert_single(
            ":a b c",
            Parens(vec![src(ColonPrefix(s("a"))), s("b"), s("c")]),
        );
        assert_single(
            "a :b c",
            Parens(vec![s("a"), src(ColonPrefix(s("b"))), s("c")]),
        );
        assert_single("::a", ColonPrefix(src(ColonPrefix(s("a")))));
    }

    #[test]
    fn hash_prefix() {
        assert_single(
            "a #(b c) d",
            Parens(vec![
                s("a"),
                src(Hash(src(Parens(vec![s("b"), s("c")])))),
                s("d"),
            ]),
        );
        assert_single("a #b d", Parens(vec![s("a"), src(Hash(s("b"))), s("d")]));
        assert_single(
            "a #^b d",
            Parens(vec![s("a"), src(Hash(src(Caret(s("b"))))), s("d")]),
        );
        assert_single(
            "a #[b (e f)] d",
            Parens(vec![
                s("a"),
                src(Hash(src(Bracket(vec![src(Parens(vec![
                    s("b"),
                    src(Parens(vec![s("e"), s("f")])),
                ]))])))),
                s("d"),
            ]),
        );
        assert_single("#a b d", Parens(vec![src(Hash(s("a"))), s("b"), s("d")]));
        assert_fail("a ^#b d");
        assert_fail("a #(b c d");
    }

    #[test]
    fn double_hash_prefix() {
        assert_single(
            "a ##(b c) d",
            Parens(vec![
                s("a"),
                src(DoubleHash(src(Parens(vec![s("b"), s("c")])))),
                s("d"),
            ]),
        );
        assert_single(
            "a ##b d",
            Parens(vec![s("a"), src(DoubleHash(s("b"))), s("d")]),
        );
        assert_single(
            "a ##^b d",
            Parens(vec![s("a"), src(DoubleHash(src(Caret(s("b"))))), s("d")]),
        );
        assert_single(
            "a ##[b (e f)] d",
            Parens(vec![
                s("a"),
                src(DoubleHash(src(Bracket(vec![src(Parens(vec![
                    s("b"),
                    src(Parens(vec![s("e"), s("f")])),
                ]))])))),
                s("d"),
            ]),
        );
        assert_single(
            "##a b d",
            Parens(vec![src(DoubleHash(s("a"))), s("b"), s("d")]),
        );
        assert_fail("a ^##b d");
        assert_fail("a ##(b c d");
    }

    #[test]
    fn equal() {
        assert_single("a = b", Equal(s("a"), s("b")));
    }

    #[test]
    fn arrow() {
        assert_single("a -> b", Arrow(s("a"), s("b")));
    }

    #[test]
    fn colon() {
        assert_single("a:b", Colon(s("a"), s("b")));
        assert_fail("a:");
        assert_single("a:b:c", Colon(src(Colon(s("a"), s("b"))), s("c")));
        assert_single(
            ":a:b::c",
            Colon(
                src(Colon(src(ColonPrefix(s("a"))), s("b"))),
                src(ColonPrefix(s("c"))),
            ),
        );
        assert_single(
            "a b:c :d",
            Parens(vec![
                s("a"),
                src(Colon(s("b"), s("c"))),
                src(ColonPrefix(s("d"))),
            ]),
        );
    }

    #[test]
    fn pipe_sugar() {
        assert_same("a b |> c d", "(a b) c d");
        assert_same("a |> b c", "a b c");
        assert_same("a b | c d", "c d (a b)");
        assert_same("a | c d", "c d a");
        assert_same("a b <| c d", "a b (c d)");
        assert_same("a b <| c", "a b c");
        assert_same(
            "a | b <| c <| d | e |> f <| g | h",
            "b (c ((e d) f (h g))) a",
        );
        assert_same("a b |>:<| c d", "(a b):(c d)");
        assert_same("a b <|", "a b ()");
        assert_same("a b |>", "a b");
        assert_same("<| a b", "a b");
    }

    #[test]
    fn pipe_colon() {
        assert_same("a b |>:c d", "(a b):c d");
        assert_same("a b |>:c | d", "d (a b):c");
        assert_same("a b:<| c d", "a b:(c d)");
        assert_same("a b |>:c:d", "(a b):c:d");
    }

    #[test]
    fn parens() {
        assert_single("a b", Parens(vec![s("a"), s("b")]));
        assert_single("((a))", String("a".into()));
    }

    #[test]
    fn empty_parens() {
        assert_single("()", Parens(vec![]));
    }

    #[test]
    fn curly() {
        assert_single(
            "{a b
        c d; e f, g h
        }",
            Curly(vec![
                src(Parens(vec![s("a"), s("b")])),
                src(Parens(vec![s("c"), s("d")])),
                src(Parens(vec![s("e"), s("f")])),
                src(Parens(vec![s("g"), s("h")])),
            ]),
        )
    }

    #[test]
    fn bracket() {
        assert_single(
            "[a b
        c d; e f, g h
        ]",
            Bracket(vec![
                src(Parens(vec![s("a"), s("b")])),
                src(Parens(vec![s("c"), s("d")])),
                src(Parens(vec![s("e"), s("f")])),
                src(Parens(vec![s("g"), s("h")])),
            ]),
        )
    }

    #[test]
    fn quote() {
        assert_single(
            "\"hello world\"",
            Quote(vec![src(StringTree::String("hello world".into()))]),
        );
    }

    #[test]
    fn quote_unicode_escape() {
        assert_single(
            r#""my \u{65}scape""#,
            Quote(vec![src(StringTree::String("my escape".into()))]),
        );
    }

    #[test]
    fn quote_invalid_unicode_escape() {
        assert_err(r#""\u{FFFFFF}""#, Error::InvalidUnicodeCharacter);
        assert_err(r#""\u{hi}""#, Error::InvalidUnicodeCharacter);
        assert_err(r#""\u{}""#, Error::InvalidUnicodeCharacter);
        assert_err(r#""\u{1234512345}""#, Error::InvalidUnicodeCharacter);
    }

    #[test]
    fn quote_unclosed_unicode_escape() {
        assert_err(r#""\u{1234""#, Error::UnfinishedUnicodeCharacter);
    }

    #[test]
    fn quote_bad_escape() {
        assert_err("\"ohn\\o\"", Error::UnrecognizedEscapeSequence);
    }

    #[test]
    fn quote_nested() {
        assert_single(
            "\"hello $(get-name :name)!\"",
            Quote(vec![
                src(StringTree::String("hello ".into())),
                src(StringTree::Expression(src(Parens(vec![
                    s("get-name"),
                    src(ColonPrefix(s("name"))),
                ])))),
                src(StringTree::String("!".into())),
            ]),
        );
    }

    #[test]
    fn apostrophe_space_block() {
        assert_single(
            "'\n  ' block string\n  ' here",
            ApostropheBlock(vec![
                src(StringTree::String("block string\n".into())),
                src(StringTree::String("here".into())),
            ]),
        );
    }

    #[test]
    fn apostrophe_space_block_nested() {
        assert_single(
            "'\n  ' block $$$string\n  ' here",
            ApostropheBlock(vec![
                src(StringTree::String("block ".into())),
                src(StringTree::String("$".into())),
                src(StringTree::Expression(s("string"))),
                src(StringTree::String("\n".into())),
                src(StringTree::String("here".into())),
            ]),
        );
    }

    #[test]
    fn hash_space_block() {
        assert_single(
            "# hello world\n# this is a comment $$$hi",
            HashBlock("hello world\nthis is a comment $$$hi".into()),
        );
    }

    #[test]
    fn double_hash_space_block() {
        assert_single(
            "##\n  ## block string\n  ## here",
            DoubleHashBlock(vec![
                src(StringTree::String("block string\n".into())),
                src(StringTree::String("here".into())),
            ]),
        );
    }

    #[test]
    fn double_hash_space_block_nested() {
        assert_single(
            "##\n  ## block $$$string\n  ## here",
            DoubleHashBlock(vec![
                src(StringTree::String("block ".into())),
                src(StringTree::String("$".into())),
                src(StringTree::Expression(s("string"))),
                src(StringTree::String("\n".into())),
                src(StringTree::String("here".into())),
            ]),
        );
    }

    #[test]
    fn unary_no_arg() {
        for op in &["$", ":", "^"] {
            assert_fail(op);
        }
    }

    #[test]
    fn binary_no_arg() {
        for op in &["->", "="] {
            assert_err(op, Error::BinaryMissingPrevious);
            assert_err(format!("a {}", op).as_str(), Error::BinaryMissingNext);
            assert_err(format!("{} a", op).as_str(), Error::BinaryMissingPrevious);
        }
    }

    #[test]
    fn mixed() {
        assert_single(
            ":a = g <| a:b $c -> a <| b c",
            Equal(
                src(ColonPrefix(s("a"))),
                src(Parens(vec![
                    s("g"),
                    src(Arrow(
                        src(Parens(vec![
                            src(Colon(s("a"), s("b"))),
                            src(Dollar(s("c"))),
                        ])),
                        src(Parens(vec![s("a"), src(Parens(vec![s("b"), s("c")]))])),
                    )),
                ])),
            ),
        )
    }

    fn assert_single(s: &str, expected: Tree) {
        let elements = single(s);
        dbg!(&elements);
        assert!(elements == expected);
    }

    fn single(s: &str) -> Tree {
        Parser::from(Tokens::from(Source::missing(s)))
            .next()
            .unwrap()
            .unwrap()
            .unwrap()
    }

    fn assert_fail(s: &str) {
        Parser::from(Tokens::from(Source::missing(s)))
            .next()
            .unwrap()
            .unwrap_err();
    }

    fn assert_err(s: &str, e: Error<super::super::tokenize::Error>) {
        let got = Parser::from(Tokens::from(Source::missing(s)))
            .next()
            .unwrap()
            .unwrap_err()
            .pop()
            .unwrap()
            .unwrap();
        dbg!(&got);
        assert!(e == got);
    }

    fn assert_same(a: &str, b: &str) {
        let a = single(a);
        let b = single(b);
        dbg!(&a, &b);
        assert!(a == b);
    }
}
