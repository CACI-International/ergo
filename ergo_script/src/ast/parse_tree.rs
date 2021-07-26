//! Parsing of tree tokens.
//!
//! Pipe operators are desugared and the syntax tree is formed.

use super::tokenize_tree::{PairedToken, SymbolicToken, TreeToken};
use ergo_runtime::{source::IntoSource, Source};
use std::fmt;

pub type TreeVec = Vec<Source<Tree>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Tree {
    String(String, bool),
    DocString(String),
    Bang(Box<Source<Tree>>),
    Caret(Box<Source<Tree>>),
    ColonPrefix(Box<Source<Tree>>),
    ColonSuffix(Box<Source<Tree>>),
    Equal(Box<Source<Tree>>, Box<Source<Tree>>),
    Arrow(Box<Source<Tree>>, Box<Source<Tree>>),
    Colon(Box<Source<Tree>>, Box<Source<Tree>>),
    Parens(TreeVec),
    Curly(TreeVec),
    Bracket(TreeVec),
    DocCurly(TreeVec),
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
                        SymbolicToken::String { string, quoted } => {
                            Tree::String(string, quoted).into()
                        }
                        SymbolicToken::DocString(s) => Tree::DocString(s).into(),
                        o => o.into(),
                    }),
                    TreeToken::ColonPriorFree => source.with(TreeOrSymbol::ColonPriorFree),
                    TreeToken::ColonPostFree => source.with(TreeOrSymbol::ColonPostFree),
                    TreeToken::StartNested(p) => {
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
                            PairedToken::Curly => {
                                let groups = self.groups()?;
                                // Elaborate solitary literal strings `a` to `:a = :a`.
                                let groups = groups
                                    .into_iter()
                                    .map(|v| {
                                        let (source, v) = v.take();
                                        match v {
                                            Tree::String(s, false) => {
                                                let colon = Box::from(
                                                    source.clone().with(Tree::ColonPrefix(
                                                        source
                                                            .clone()
                                                            .with(Tree::String(s, false))
                                                            .into(),
                                                    )),
                                                );
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
                            // It's not really useful to elaborate strings in doc curlies, so we
                            // won't treat them quite the same as curlies.
                            PairedToken::DocCurly => Tree::DocCurly(self.groups()?),
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

        // Read all tokens until end of current group, next child separator, or doc tokens
        // (which should be considered separate groups).
        while self
            .tokens
            .peek()
            .map(|t| {
                t.as_ref()
                    .map(|t| {
                        *t != TreeToken::EndNested
                            && *t != TreeToken::NextChild
                            && (parts.is_empty() || !t.is_doc_token())
                    })
                    .unwrap_or(true)
            })
            .unwrap_or(false)
        {
            parts.push(self.tree_or_symbol()?.unwrap());
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
        //   Bang/Caret Prefix
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
                |_, t| Tree::ColonPrefix(t.into()),
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
                        .map(|(a, b)| Tree::Colon(a.into(), b.into()));
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
                |_, t| Tree::ColonSuffix(t.into()),
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

        fn bang_prefix_group<E>(toks: &mut Toks) -> TResult<E> {
            prefixed_initial(toks, &SymbolicToken::Bang, bang_caret_prefixes, Tree::Bang)
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
    use ergo_runtime::source::StringSource;

    use Tree::*;

    fn src<T, R>(e: T) -> R
    where
        R: From<Source<T>>,
    {
        Source::builtin(e).into()
    }

    fn s<R>(s: &str) -> R
    where
        R: From<Source<Tree>>,
    {
        src(String(s.into(), false))
    }

    #[test]
    fn string() {
        assert_single("s", String("s".into(), false));
        assert_single("\"hello world\"", String("hello world".into(), true));
    }

    #[test]
    fn doc_comment() {
        assert_single(
            "# some comment\n##doc comment\n",
            DocString("doc comment".into()),
        );
    }

    #[test]
    fn parens() {
        assert_single("a b", Parens(vec![s("a"), s("b")]));
        assert_single("((a))", String("a".into(), false));
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
        assert_single("!a", Bang(s("a")));
        assert_single("!a b c", Bang(src(Parens(vec![s("a"), s("b"), s("c")]))));
        assert_single("a !b c", Parens(vec![s("a"), src(Bang(s("b"))), s("c")]));
    }

    #[test]
    fn caret() {
        assert_single("^a", Caret(s("a")));
        assert_single("^a b c", Caret(src(Parens(vec![s("a"), s("b"), s("c")]))));
        assert_single("a ^b c", Parens(vec![s("a"), src(Caret(s("b"))), s("c")]));
    }

    #[test]
    fn colon() {
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
    fn colon_ops() {
        assert_single("a:b", Colon(s("a"), s("b")));
        assert_single("a:", ColonSuffix(s("a")));
        assert_single("a:b:c", Colon(src(Colon(s("a"), s("b"))), s("c")));
        assert_single(
            ":a:b::c:",
            ColonSuffix(src(Colon(
                src(Colon(src(ColonPrefix(s("a"))), s("b"))),
                src(ColonPrefix(s("c"))),
            ))),
        );
        assert_single(
            "a b:c d:e:",
            Parens(vec![
                s("a"),
                src(Colon(s("b"), s("c"))),
                src(ColonSuffix(src(Colon(s("d"), s("e"))))),
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
        assert_same("a b |>:", "(a b):");
    }

    #[test]
    fn pipe_colon() {
        assert_same("a b |>:c d", "(a b):c d");
        assert_same("a b |>:c | d", "d (a b):c");
        assert_same("a b:<| c d", "a b:(c d)");
        assert_same("a b |>:c:d", "(a b):c:d");
    }

    #[test]
    fn equal() {
        assert_single("a = b", Equal(s("a"), s("b")));
    }

    #[test]
    fn force_equal() {
        assert_single("a = b", Equal(s("a"), s("b")));
    }

    #[test]
    fn arrow() {
        assert_single("a -> b", Arrow(s("a"), s("b")));
    }

    #[test]
    fn mixed() {
        assert_single(
            ":a = g <| a:b :c -> a <| b c",
            Equal(
                src(ColonPrefix(s("a"))),
                src(Parens(vec![
                    s("g"),
                    src(Arrow(
                        src(Parens(vec![
                            src(Colon(s("a"), s("b"))),
                            src(ColonPrefix(s("c"))),
                        ])),
                        src(Parens(vec![s("a"), src(Parens(vec![s("b"), s("c")]))])),
                    )),
                ])),
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
        for op in &["->", "="] {
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
            Source::new(StringSource::new("<string>", s.to_owned()))
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
            Source::new(StringSource::new("<string>", s.to_owned()))
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
