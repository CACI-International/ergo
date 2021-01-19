//! Parsing of tree tokens.
//!
//! Pipe operators and (infix) colons are desugared, and infix operators are disambiguated.

use super::tokenize_tree::{PairedToken, SymbolicToken, TreeToken};
use ergo_runtime::{source::IntoSource, Source};
use std::fmt;

pub type TreeVec = Vec<Source<Tree>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Tree {
    String(String),
    DocComment(String),
    Bang(Box<Source<Tree>>),
    Caret(Box<Source<Tree>>),
    Colon(Box<Source<Tree>>),
    Equal(Box<Source<Tree>>, Box<Source<Tree>>),
    Arrow(Box<Source<Tree>>, Box<Source<Tree>>),
    Parens(TreeVec),
    Curly(TreeVec),
    Bracket(TreeVec),
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
    /// A unary operator is missing its argument (which should be before it).
    UnaryMissingPrefix,
    /// A unary operator is missing its argument (which should be after it).
    UnaryMissingSuffix,
    /// A binary operator is missing its previous argument.
    BinaryMissingPrevious,
    /// A binary operator is missing its next argument.
    BinaryMissingNext,
    Multiple(Vec<Source<Error<TokError>>>),
}

impl<E: fmt::Display> fmt::Display for Error<E> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Tokenization(t) => write!(f, "{}", t),
            Error::UnaryMissingPrefix => write!(f, "expected an argument before the operator"),
            Error::UnaryMissingSuffix => write!(f, "expected an argument after the operator"),
            Error::BinaryMissingPrevious => write!(f, "expected an argument before the operator"),
            Error::BinaryMissingNext => write!(f, "expected an argument after the operator"),
            Error::Multiple(es) => {
                let mut es = es.into_iter();
                if let Some(e) = es.next() {
                    write!(f, "{}", e)?;
                }
                for e in es {
                    write!(f, "\n{}", e)?;
                }
                Ok(())
            }
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

#[derive(Debug, Clone, PartialEq, Eq)]
enum TreeOrSymbol {
    Tree(Tree),
    Symbol(SymbolicToken),
    ColonPriorFree,
    ColonPostFree,
    PipeRightColon,
    ColonPipeLeft,
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

    pub fn as_tree(self) -> Tree {
        match self {
            TreeOrSymbol::Tree(t) => t,
            _ => panic!("not a tree"),
        }
    }

    pub fn is_front_colon(&self) -> bool {
        self == &TreeOrSymbol::ColonPriorFree
    }

    pub fn is_back_colon(&self) -> bool {
        self == &TreeOrSymbol::ColonPostFree
    }

    pub fn is_leftward_pipe(&self) -> bool {
        self == &TreeOrSymbol::ColonPipeLeft || self.is_symbol(&SymbolicToken::PipeLeft)
    }

    pub fn is_rightward_pipe(&self) -> bool {
        self == &TreeOrSymbol::PipeRightColon
            || self.is_symbol(&SymbolicToken::Pipe)
            || self.is_symbol(&SymbolicToken::PipeRight)
    }
}

impl<T: IntoIterator> From<T> for Parser<T::IntoIter> {
    fn from(iter: T) -> Self {
        Parser {
            tokens: iter.into_iter().peekable(),
        }
    }
}

impl<I, E: fmt::Debug> Iterator for Parser<I>
where
    I: Iterator<Item = Result<Source<TreeToken>, Source<E>>>,
{
    type Item = Result<Source<Tree>, Source<Error<E>>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.tokens.peek().is_none() {
            None
        } else {
            match self.group_as_tree() {
                Ok(Some(v)) => Some(Ok(v)),
                Ok(None) => None,
                Err(e) => Some(Err(e)),
            }
        }
    }
}

impl<I, E: fmt::Debug> Parser<I>
where
    I: Iterator<Item = Result<Source<TreeToken>, Source<E>>>,
{
    fn next_tok(&mut self) -> Result<Option<Source<TreeToken>>, Source<Error<E>>> {
        match self.tokens.next() {
            None => Ok(None),
            Some(Err(e)) => Err(e.map(Error::Tokenization)),
            Some(Ok(t)) => Ok(Some(t)),
        }
    }

    fn tree_or_symbol(&mut self) -> Result<Option<Source<TreeOrSymbol>>, Source<Error<E>>> {
        match self.next_tok()? {
            None => Ok(None),
            Some(t) => {
                let (source, t) = t.take();
                Ok(Some(match t {
                    TreeToken::Symbol(s) => source.with(match s {
                        SymbolicToken::String(s) => Tree::String(s).into(),
                        SymbolicToken::DocComment(s) => Tree::DocComment(s).into(),
                        o => o.into(),
                    }),
                    TreeToken::ColonPriorFree => source.with(TreeOrSymbol::ColonPriorFree),
                    TreeToken::ColonPostFree => source.with(TreeOrSymbol::ColonPostFree),
                    TreeToken::StartNested(p) => {
                        let mut src = None;
                        let t = match p {
                            PairedToken::Paren => {
                                let inner = self.group()?;
                                loop {
                                    // If inner is an arrow or equal, the parens just imply grouping.
                                    if inner.len() == 1 {
                                        let f = inner.first().unwrap();
                                        match AsRef::as_ref(f) {
                                            Tree::Arrow(_, _) | Tree::Equal(_, _) => {
                                                let (s, inner) =
                                                    inner.into_iter().next().unwrap().take();
                                                src = Some(s);
                                                break inner;
                                            }
                                            _ => (),
                                        }
                                    }
                                    break Tree::Parens(inner);
                                }
                            }
                            PairedToken::Curly => {
                                let groups = self.groups()?;
                                // Elaborate solitary literal strings `a` to `:a = :a`.
                                let groups = groups
                                    .into_iter()
                                    .map(|v| {
                                        let (source, v) = v.take();
                                        match v {
                                            Tree::String(s) => {
                                                let colon =
                                                    Box::from(source.clone().with(Tree::Colon(
                                                        source.clone().with(Tree::String(s)).into(),
                                                    )));
                                                source
                                                    .with(Tree::Equal(colon.clone(), colon.clone()))
                                            }
                                            o => source.with(o),
                                        }
                                    })
                                    .collect();
                                Tree::Curly(groups)
                            }
                            PairedToken::Bracket => Tree::Bracket(self.groups()?),
                        };
                        // Next token must be valid and EndNested
                        let end = self.next_tok().unwrap().unwrap();
                        debug_assert!(*end == TreeToken::EndNested);
                        src.unwrap_or_else(|| (source, end).into_source().source())
                            .with(t.into())
                    }
                    TreeToken::EndNested => panic!("unexpected token"),
                    TreeToken::NextChild => panic!("unexpected token"),
                }))
            }
        }
    }

    fn group(&mut self) -> Result<TreeVec, Source<Error<E>>> {
        let mut parts: Vec<Source<TreeOrSymbol>> = Default::default();

        // Consume extra NextChild without any content
        while self
            .tokens
            .peek()
            .map(|t| {
                t.as_ref()
                    .map(|t| *t == TreeToken::NextChild)
                    .unwrap_or(false)
            })
            .unwrap_or(false)
        {
            self.tokens.next();
        }

        while self
            .tokens
            .peek()
            .map(|t| {
                t.as_ref()
                    .map(|t| *t != TreeToken::EndNested && *t != TreeToken::NextChild)
                    .unwrap_or(true)
            })
            .unwrap_or(false)
        {
            let mut n = self.tree_or_symbol()?.unwrap();

            // Replace `|>:` and `:<|` with special operators, to make the following parsing easier
            // and consistent with other binary operator parsing.
            if let Some(last) = parts.last() {
                if last.is_symbol(&SymbolicToken::PipeRight) && n.is_symbol(&SymbolicToken::Colon) {
                    let old = parts.pop().unwrap();
                    n = (old, n).into_source().with(TreeOrSymbol::PipeRightColon);
                } else if last.is_symbol(&SymbolicToken::Colon)
                    && n.is_symbol(&SymbolicToken::PipeLeft)
                {
                    let old = parts.pop().unwrap();
                    n = (old, n).into_source().with(TreeOrSymbol::ColonPipeLeft);
                }
            }

            parts.push(n);
        }

        if parts.is_empty() {
            return Ok(vec![]);
        }

        // Elaborate [`Colon`,`ColonPostFree`] to [`ColonPostFree`, `ColonPostFree`] so multiple
        // trailing colons works as expected.
        {
            let mut make_suffix = false;
            for t in parts.iter_mut().rev() {
                if make_suffix && **t == TreeOrSymbol::Symbol(SymbolicToken::Colon) {
                    **t = TreeOrSymbol::ColonPostFree;
                }
                make_suffix = **t == TreeOrSymbol::ColonPostFree;
            }
        }

        // Resolve operators
        // Precedence (descending):
        //   Colon Prefix
        //   Colon (Left assoc)
        //   Colon Suffix
        //   Caret Prefix
        //   PipeLeft (Right assoc)
        //   Pipe/PipeRight (Left assoc)
        //   Bang Expression Prefix
        //   Arrow (Right assoc)
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
                |t| t.is_front_colon(),
                to_treedeque,
                |_, t| Tree::Colon(t.into()),
            )
        }

        fn colon<E>(toks: &mut Toks) -> TResult<E> {
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
                        .map(|(a, b)| Tree::Parens(vec![a, b]));
                    ret.push_back(c);
                    ret.extend(b);
                    ret
                },
            )
        }

        fn colon_suffix<E>(toks: &mut Toks) -> TResult<E> {
            suffix_op(
                toks,
                |t| t.is_back_colon(),
                colon,
                |_, t| Tree::Parens(vec![t]),
            )
        }

        fn bang_caret_prefixes<E>(toks: &mut Toks) -> TResult<E> {
            prefix_op(
                toks,
                |t| t.is_symbol(&SymbolicToken::Bang) || t.is_symbol(&SymbolicToken::Caret),
                colon_suffix,
                |s, t| match s {
                    TreeOrSymbol::Symbol(SymbolicToken::Bang) => Tree::Bang(t.into()),
                    TreeOrSymbol::Symbol(SymbolicToken::Caret) => Tree::Caret(t.into()),
                    _ => panic!("unexpected symbol"),
                },
            )
        }

        fn pipe_left<E>(toks: &mut Toks) -> TResult<E> {
            right_bin_op(
                toks,
                |v| v.is_leftward_pipe(),
                bang_caret_prefixes,
                |o, a, b| {
                    let mut ret = a;
                    let b = b.into_source().map(|v| Tree::Parens(v.into()));
                    match o {
                        TreeOrSymbol::Symbol(SymbolicToken::PipeLeft) => ret.push_back(b),
                        TreeOrSymbol::ColonPipeLeft => {
                            let back_a = ret.pop_back().unwrap();
                            ret.push_back(
                                (back_a, b)
                                    .into_source()
                                    .map(|(back_a, b)| Tree::Parens(vec![back_a, b])),
                            );
                        }
                        _ => panic!("unexpected token"),
                    }
                    ret
                },
            )
        }

        fn pipe_right<E>(toks: &mut Toks) -> TResult<E> {
            let next = pipe_left;

            let split = toks.iter().rposition(|t| t.is_rightward_pipe());
            if let Some(pos) = split {
                let (a, b) = toks.split_at_mut(pos);
                let (t, b) = b.split_at_mut(1);
                let t = unsafe { t.get_unchecked_mut(0) }.as_mut().map(|t| t.take());

                if a.is_empty() {
                    return Err(t.with(Error::BinaryMissingPrevious));
                } else if b.is_empty() {
                    return Err(t.with(Error::BinaryMissingNext));
                }

                let (t_source, t) = t.take();

                let a = pipe_right(a)?.into_source().map(|v| Tree::Parens(v.into()));
                match t {
                    TreeOrSymbol::Symbol(SymbolicToken::Pipe) => {
                        let mut ret = next(b)?;
                        ret.push_back(a);
                        Ok(ret)
                    }
                    TreeOrSymbol::Symbol(SymbolicToken::PipeRight) => {
                        let mut ret = next(b)?;
                        ret.push_front(a);
                        Ok(ret)
                    }
                    TreeOrSymbol::PipeRightColon => {
                        let mut toks: Vec<_> = std::iter::once(a.map(TreeOrSymbol::Tree))
                            .chain(std::iter::once(
                                t_source.with(TreeOrSymbol::Symbol(SymbolicToken::Colon)),
                            ))
                            .chain(b.iter_mut().map(|sv| sv.as_mut().map(|v| v.take())))
                            .collect();
                        next(&mut toks)
                    }
                    _ => panic!("unexpected token"),
                }
            } else {
                next(toks)
            }
        }

        fn bang_prefix_group<E>(toks: &mut Toks) -> TResult<E> {
            prefixed_initial(toks, &SymbolicToken::Bang, pipe_right, Tree::Bang)
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

        fn eq<E>(toks: &mut Toks) -> TResult<E> {
            left_bin_op(
                toks,
                |v| v.is_symbol(&SymbolicToken::Equal),
                arrow,
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

        caret_prefix(&mut parts).map(|v| v.into())
    }

    fn group_as_tree(&mut self) -> Result<Option<Source<Tree>>, Source<Error<E>>> {
        let tv = self.group()?;
        if tv.len() == 0 {
            Ok(None)
        } else if tv.len() == 1 {
            Ok(Some(tv.into_iter().next().unwrap()))
        } else {
            Ok(Some(tv.into_source().map(Tree::Parens)))
        }
    }

    fn groups(&mut self) -> Result<TreeVec, Source<Error<E>>> {
        let mut result = Vec::new();
        let mut errors = Vec::new();

        while self
            .tokens
            .peek()
            .map(|t| {
                t.as_ref()
                    .map(|t| *t != TreeToken::EndNested)
                    .unwrap_or(true)
            })
            .unwrap_or(false)
        {
            match self.group_as_tree() {
                Ok(Some(v)) => result.push(v),
                Ok(None) => (),
                Err(e) => errors.push(e),
            }
        }

        if errors.is_empty() {
            Ok(result)
        } else {
            Err(if errors.len() == 1 {
                errors.into_iter().next().unwrap()
            } else {
                result.into_source().with(Error::Multiple(errors))
            })
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::tokenize::Tokens;
    use crate::ast::tokenize_tree::TreeTokens;

    use Tree::*;

    fn src<T>(e: T) -> Source<T> {
        Source::builtin(e)
    }

    fn s(s: &str) -> Source<Tree> {
        src(String(s.into()))
    }

    #[test]
    fn string() {
        assert_single("s", String("s".into()));
        assert_single("\"hello world\"", String("hello world".into()));
    }

    #[test]
    fn doc_comment() {
        assert_single(
            "# some comment\n## doc comment\n",
            DocComment("doc comment".into()),
        );
    }

    #[test]
    fn parens() {
        assert_single("a b", Parens(vec![s("a"), s("b")]));
        assert_single("((a))", Parens(vec![src(Parens(vec![s("a")]))]));
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
    fn curly_shorthand() {
        assert_same("{a,b,:c=1}", "{:a=:a,:b=:b,:c=1}");
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
    fn bang() {
        assert_single("!a", Bang(s("a").into()));
        assert_single(
            "!a b c",
            Bang(src(Parens(vec![s("a"), s("b"), s("c")])).into()),
        );
        assert_single(
            "a !b c",
            Parens(vec![s("a"), src(Bang(s("b").into())), s("c")]),
        );
    }

    #[test]
    fn caret() {
        assert_single("^a", Caret(s("a").into()));
        assert_single(
            "^a b c",
            Caret(src(Parens(vec![s("a"), s("b"), s("c")])).into()),
        );
        assert_single(
            "a ^b c",
            Parens(vec![s("a"), src(Caret(s("b").into())), s("c")]),
        );
    }

    #[test]
    fn colon() {
        assert_single(":a", Colon(s("a").into()));
        assert_single(
            ":a b c",
            Parens(vec![src(Colon(s("a").into())), s("b"), s("c")]),
        );
        assert_single(
            "a :b c",
            Parens(vec![s("a"), src(Colon(s("b").into())), s("c")]),
        );
        assert_single("::a", Colon(src(Colon(s("a").into())).into()));
    }

    #[test]
    fn colon_command_sugar() {
        assert_same("a:b", "(a b)");
        assert_same("a:", "(a)");
        assert_same("a:b:c", "((a b) c)");
        assert_same(":a:b::c:", "(((:a b) :c))");
        assert_same("a b:c d:e:", "a (b c) ((d e))");
    }

    #[test]
    fn pipe_sugar() {
        assert_same("a b |> c d", "(a b) c d");
        assert_same("a b | c d", "c d (a b)");
        assert_same("a b <| c d", "a b (c d)");
        assert_same(
            "a | b <| c <| d | e |> f <| g | h",
            "h ((e (b (c (d)) (a))) f (g))",
        );
    }

    #[test]
    fn pipe_colon() {
        assert_same("a b |>:c d", "((a b) c) d");
        assert_same("a b:<| c d", "a (b (c d))");
        assert_same("a b |>:c:d", "((a b) c) d");
    }

    #[test]
    fn equal() {
        assert_single("a = b", Equal(s("a").into(), s("b").into()));
    }

    #[test]
    fn force_equal() {
        assert_single("a = b", Equal(s("a").into(), s("b").into()));
    }

    #[test]
    fn arrow() {
        assert_single("a -> b", Arrow(s("a").into(), s("b").into()));
    }

    #[test]
    fn mixed() {
        assert_single(
            ":a = a:b :c -> a <| b c",
            Equal(
                src(Colon(s("a").into())).into(),
                src(Arrow(
                    src(Parens(vec![
                        src(Parens(vec![s("a"), s("b")])),
                        src(Colon(s("c").into())),
                    ]))
                    .into(),
                    src(Parens(vec![s("a"), src(Parens(vec![s("b"), s("c")]))])).into(),
                ))
                .into(),
            ),
        )
    }

    #[test]
    fn unary_no_arg() {
        for op in &[":", "!", "^"] {
            assert_fail(op);
        }
    }

    #[test]
    fn binary_no_arg() {
        for op in &["|>", "|", "<|", "->", "="] {
            assert_fail(op);
            assert_fail(format!("a {}", op).as_str());
            assert_fail(format!("{} a", op).as_str());
        }
    }

    fn assert_single(s: &str, expected: Tree) {
        let elements = single(s);
        dbg!(&elements);
        assert!(elements == expected);
    }

    fn single(s: &str) -> Tree {
        Parser::from(TreeTokens::from(Tokens::from(
            Source::new(super::super::StringSource::new("<string>", s.to_owned()))
                .open()
                .unwrap(),
        )))
        .next()
        .unwrap()
        .unwrap()
        .unwrap()
    }

    fn assert_fail(s: &str) {
        Parser::from(TreeTokens::from(Tokens::from(
            Source::new(super::super::StringSource::new("<string>", s.to_owned()))
                .open()
                .unwrap(),
        )))
        .next()
        .unwrap()
        .unwrap_err();
    }

    fn assert_same(a: &str, b: &str) {
        let a = single(a);
        let b = single(b);
        dbg!(&a, &b);
        assert!(a == b);
    }
}
