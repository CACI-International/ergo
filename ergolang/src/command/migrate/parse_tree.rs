//! Tree parsing.
//!
//! A basic syntax tree is formed, and in the process:
//! * Pipe operators are desugared.
//! * Redundant groupings (parens) are removed.
//! * Quoted string escape sequences are interpreted.

use super::tokenize::{PairedToken, SymbolicToken, Token};
use ergo_runtime::{source::IntoSource, Source};
use std::fmt;

pub type TreeVec = Vec<Source<Tree>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Tree {
    String(String),
    Bang(Box<Source<Tree>>),
    Caret(Box<Source<Tree>>),
    ColonPrefix(Box<Source<Tree>>),
    Hash(Box<Source<Tree>>),
    DoubleHash(Box<Source<Tree>>),
    Equal(Box<Source<Tree>>, Box<Source<Tree>>),
    Arrow(Box<Source<Tree>>, Box<Source<Tree>>),
    Colon(Box<Source<Tree>>, Box<Source<Tree>>),
    // Groups
    Parens(TreeVec),
    Curly(TreeVec),
    Bracket(TreeVec),
    // String Groups
    Quote(StringTreeVec),
    ApostropheSpace(StringTreeVec),
    HashSpace(String),
    DoubleHashSpace(StringTreeVec),
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

impl<'a, I, E: fmt::Debug> Iterator for Parser<I>
where
    I: Iterator<Item = Result<Source<Token<'a>>, Source<E>>>,
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

fn parse_escape_sequences<E>(s: &str) -> Result<String, (Error<E>, usize, usize)> {
    let mut ret = String::with_capacity(s.len());
    let mut escape = None;

    let mut chars = s.char_indices();
    while let Some((offset, c)) = chars.next() {
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
                        if let Some((offset, '{')) = chars.next() {
                            let unicode_hex = chars.as_str();
                            let mut end = false;
                            while let Some((_, c)) = chars.next() {
                                if c == '}' {
                                    end = true;
                                    break;
                                }
                            }
                            if !end {
                                return Err((
                                    Error::UnfinishedUnicodeCharacter,
                                    start_offset,
                                    s.len(),
                                ));
                            } else {
                                let unicode_hex_len =
                                    unicode_hex.len() - chars.as_str().len() - '}'.len_utf8();
                                // SAFETY: end must be in bounds and on a character boundary
                                // according to the character iterator.
                                let unicode_hex =
                                    unsafe { unicode_hex.get_unchecked(..unicode_hex_len) };
                                let end_offset =
                                    offset + '{'.len_utf8() + unicode_hex_len + '}'.len_utf8();
                                ret.push(
                                    u32::from_str_radix(unicode_hex, 16)
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
    }

    Ok(ret)
}

impl<'a, I, E: fmt::Debug> Parser<I>
where
    I: Iterator<Item = Result<Source<Token<'a>>, Source<E>>>,
{
    fn next_tok(&mut self) -> Result<Option<Source<Token<'a>>>, Source<Error<E>>> {
        match self.tokens.next() {
            None => Ok(None),
            Some(Err(e)) => Err(e.map(Error::Tokenization)),
            Some(Ok(t)) => Ok(Some(t)),
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
                            PairedToken::ApostropheSpace => {
                                Tree::ApostropheSpace(self.string_tree(false)?)
                            }
                            PairedToken::HashSpace => Tree::HashSpace(self.join_strings()?),
                            PairedToken::DoubleHashSpace => {
                                Tree::DoubleHashSpace(self.string_tree(false)?)
                            }
                        };
                        // Next token must be valid and EndNested
                        let end = self.next_tok().unwrap().unwrap();
                        debug_assert!(*end == Token::EndNested);
                        src.unwrap_or_else(|| (source, end).into_source().source())
                            .with(t.into())
                    }
                    Token::EndNested => panic!("unexpected token"),
                    Token::NextChild => panic!("unexpected token"),
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
        //   Colon Prefix
        //   Colon (Left assoc)
        //   Bang/Caret Prefix
        //   Hash/DoubleHash Prefix
        //   Bang Expression Prefix
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

        fn colon_prefix<E>(toks: &mut Toks) -> TResult<E> {
            prefix_op(
                toks,
                |v| v.is_symbol(&SymbolicToken::ColonPrefix),
                to_treedeque,
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

        fn bang_caret_prefixes<E>(toks: &mut Toks) -> TResult<E> {
            prefix_op(
                toks,
                |t| t.is_symbol(&SymbolicToken::Bang) || t.is_symbol(&SymbolicToken::Caret),
                colon,
                |s, t| match s {
                    TreeOrSymbol::Symbol(SymbolicToken::Bang) => Tree::Bang(t.into()),
                    TreeOrSymbol::Symbol(SymbolicToken::Caret) => Tree::Caret(t.into()),
                    _ => panic!("unexpected symbol"),
                },
            )
        }

        fn hash_prefixes<E>(toks: &mut Toks) -> TResult<E> {
            prefix_op(
                toks,
                |t| t.is_symbol(&SymbolicToken::Hash) || t.is_symbol(&SymbolicToken::DoubleHash),
                bang_caret_prefixes,
                |s, t| match s {
                    TreeOrSymbol::Symbol(SymbolicToken::Hash) => Tree::Hash(t.into()),
                    TreeOrSymbol::Symbol(SymbolicToken::DoubleHash) => Tree::DoubleHash(t.into()),
                    _ => panic!("unexpected symbol"),
                },
            )
        }

        fn bang_prefix_group<E>(toks: &mut Toks) -> TResult<E> {
            prefixed_initial(toks, &SymbolicToken::Bang, hash_prefixes, Tree::Bang)
        }

        fn arrow<E>(toks: &mut Toks) -> TResult<E> {
            right_bin_op(
                toks,
                |v| v.is_symbol(&SymbolicToken::Arrow),
                bang_prefix_group,
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
                    Token::Symbol(SymbolicToken::Caret) => {
                        let tree = self
                            .tree_or_symbol()?
                            .expect("caret must be followed by a token")
                            .map(|t| t.as_tree());
                        Ok(Some(
                            (source, tree.source())
                                .into_source()
                                .with(StringTree::Expression(tree)),
                        ))
                    }
                    Token::String("\n") if remove_leading_newline => Ok(None),
                    Token::String(mut s) => {
                        // Check if next token is the end
                        let remove_trailing_newline = !quoted
                            && self
                                .tokens
                                .peek()
                                .unwrap()
                                .as_ref()
                                .map(|t| t == &Token::EndNested)
                                .unwrap_or(false);
                        if remove_trailing_newline && s.ends_with('\n') {
                            // SAFETY: ends_with() guarantees the offset will be valid
                            s = unsafe { s.get_unchecked(..s.len() - '\n'.len_utf8()) };
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
        .map(|strs| strs.join(""))
    }

    fn until_end_nested<T, F>(&mut self, mut f: F) -> Result<Vec<T>, Errors<E>>
    where
        F: FnMut(&mut Self) -> Result<Option<T>, Errors<E>>,
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
