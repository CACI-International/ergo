//! Tokenization of the symbolic token tree.
//!
//! A tree is tokenized based on paired tokens (parens, curly brackets, square brackets).
//! Whitespace, newlines, commas, and semicolons are resolved and removed in the following fashion:
//!
//! * Lines (the root of the parsing) may contain whitespace-separated trees.
//! * Parens may contain whitespace- and/or newline-separated trees.
//! * Curly brackets and square brackets may contain newline-, semicolon-, and/or comma-separated
//! lines.
//!
//! Colons are disambiguated between prefix/infix/suffix.

use super::tokenize::Token;
pub use super::tokenize::{PairedToken, SymbolicToken};
use ergo_runtime::Source;
use std::fmt;

/// A tree token.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TreeToken {
    Symbol(SymbolicToken),
    /// A colon preceded by a non-value.
    ColonPriorFree,
    /// A colon succeeded by a non-value.
    ColonPostFree,
    StartNested(PairedToken),
    EndNested,
    /// Start the next child (never within a PairedToken::Paren).
    NextChild,
}

impl TreeToken {
    pub fn is_doc_token(&self) -> bool {
        match self {
            TreeToken::Symbol(SymbolicToken::DocString(_)) => true,
            TreeToken::StartNested(PairedToken::DocCurly) => true,
            _ => false,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum DocComment {
    None,
    Present,
    Ended,
}

/// Iterator producing tree tokens or errors.
pub struct TreeTokens<TokenIter: Iterator> {
    iter: std::iter::Peekable<TokenIter>,
    nested: Vec<Source<PairedToken>>,
    last_implies_value: bool,
    doc_comment: DocComment,
    doc_curly_open: bool,
}

/// Tree tokenization errors.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error<TokError> {
    /// A tokenization error.
    Tokenization(TokError),
    /// An expression separator is invalid (comma or semicolon within nested expression).
    UnexpectedExpressionSeparator,
    /// An opened PairedToken does not have a corresponding closing PairedToken.
    UnmatchedOpeningToken(PairedToken),
    /// A closing PairedToken does not match the most recently-opened PairedToken.
    UnmatchedClosingToken(PairedToken, Option<Source<PairedToken>>),
}

impl<E: fmt::Display> fmt::Display for Error<E> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Tokenization(t) => write!(f, "{}", t),
            Error::UnexpectedExpressionSeparator => write!(f, "unexpected expression separator"),
            Error::UnmatchedOpeningToken(t) => write!(f, "unmatched {}", t),
            Error::UnmatchedClosingToken(t, expected) => {
                write!(f, "unmatched {}", t)?;
                if let Some(s) = expected {
                    write!(f, " (expected match for {})", s.display_inline())?;
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

impl<I: Iterator> TreeTokens<I> {
    /// Whether child delimiters (semicolon, comma, newline) are valid.
    fn has_children(&self) -> bool {
        self.nested
            .last()
            .map(|v| v != &PairedToken::Paren)
            .unwrap_or(true)
    }
}

impl<T: IntoIterator> From<T> for TreeTokens<T::IntoIter> {
    fn from(i: T) -> Self {
        TreeTokens {
            iter: i.into_iter().peekable(),
            nested: Default::default(),
            last_implies_value: false,
            doc_comment: DocComment::None,
            doc_curly_open: false,
        }
    }
}

impl<I, E> Iterator for TreeTokens<I>
where
    I: Iterator<Item = Result<Source<Token>, Source<E>>>,
{
    type Item = Result<Source<TreeToken>, Source<Error<E>>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.iter.next() {
            None => {
                if let Some(p) = self.nested.pop() {
                    Some(Err(p.map(Error::UnmatchedOpeningToken)))
                } else {
                    None
                }
            }
            Some(Err(e)) => Some(Err(e.map(Error::Tokenization))),
            Some(Ok(source_t)) => {
                let (source, t) = source_t.take();
                let last_implies_value =
                    std::mem::replace(&mut self.last_implies_value, t.implies_value_when_before());

                // If we previously saw a doc comment and the next token will not be included in
                // the doc comment (anything except newline, whitespace, and another doc comment),
                // check that any doc curlies are closed.
                if self.doc_comment == DocComment::Ended {
                    match &t {
                        Token::Newline | Token::Whitespace | Token::DocComment => (),
                        _ => {
                            self.doc_comment = DocComment::None;
                            if self.doc_curly_open {
                                let p = self
                                    .nested
                                    .pop()
                                    .expect("tree tokenization invariant failed");
                                return Some(Err(p.map(Error::UnmatchedOpeningToken)));
                            }
                        }
                    }
                }

                match t {
                    Token::Symbol(SymbolicToken::Colon) => Some({
                        let next_implies_value = self
                            .iter
                            .peek()
                            .map(|t| {
                                t.as_ref()
                                    .map(|t| t.implies_value_when_after())
                                    .unwrap_or(false)
                            })
                            .unwrap_or(false);
                        Ok(source.with(if next_implies_value {
                            if last_implies_value {
                                TreeToken::Symbol(SymbolicToken::Colon)
                            } else {
                                TreeToken::ColonPriorFree
                            }
                        } else {
                            TreeToken::ColonPostFree
                        }))
                    }),
                    Token::Symbol(s) => Some(Ok(source.with(TreeToken::Symbol(s)))),
                    Token::Pair { which, open: true } => {
                        if which == PairedToken::DocCurly {
                            self.doc_curly_open = true;
                        }
                        self.nested.push(source.clone().with(which.clone()));
                        Some(Ok(source.with(TreeToken::StartNested(which))))
                    }
                    Token::Pair { which, open: false } => Some({
                        if which == PairedToken::DocCurly {
                            self.doc_curly_open = false;
                        }
                        let expected = self.nested.pop();
                        if Some(&which) == expected.as_ref().map(|t| &**t) {
                            Ok(source.with(TreeToken::EndNested))
                        } else {
                            Err(source.with(Error::UnmatchedClosingToken(which, expected)))
                        }
                    }),
                    Token::DocComment => {
                        self.doc_comment = DocComment::Present;
                        self.next()
                    }
                    Token::Comma | Token::Semicolon => Some({
                        if self.has_children() {
                            Ok(source.with(TreeToken::NextChild))
                        } else {
                            Err(source.with(Error::UnexpectedExpressionSeparator))
                        }
                    }),
                    Token::Newline => {
                        if self.doc_comment == DocComment::Present {
                            self.doc_comment = DocComment::Ended;
                        }
                        if self.has_children() {
                            Some(Ok(source.with(TreeToken::NextChild)))
                        } else {
                            self.next()
                        }
                    }
                    Token::Whitespace => self.next(),
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::tokenize::Tokens;
    use ergo_runtime::source::StringSource;

    use PairedToken::*;
    use SymbolicToken::*;
    use TreeToken::*;

    fn s(s: &str) -> TreeToken {
        Symbol(String(s.into()))
    }

    #[test]
    fn children() {
        assert_tokens(
            "a b c
        ->;
        b = !, c; d",
            &[
                s("a"),
                s("b"),
                s("c"),
                NextChild,
                Symbol(Arrow),
                NextChild,
                NextChild,
                s("b"),
                Symbol(Equal),
                Symbol(Bang),
                NextChild,
                s("c"),
                NextChild,
                s("d"),
            ],
        )
    }

    #[test]
    fn colons() {
        assert_tokens(":a", &[ColonPriorFree, s("a")]);
        assert_tokens("::a", &[ColonPriorFree, ColonPriorFree, s("a")]);
        assert_tokens("a :a", &[s("a"), ColonPriorFree, s("a")]);
        assert_tokens("a:", &[s("a"), ColonPostFree]);
        assert_tokens("a: b", &[s("a"), ColonPostFree, s("b")]);
        assert_tokens("a:b", &[s("a"), Symbol(Colon), s("b")]);
        assert_tokens(
            "a:b:c",
            &[s("a"), Symbol(Colon), s("b"), Symbol(Colon), s("c")],
        );
        assert_tokens("a::b", &[s("a"), Symbol(Colon), ColonPriorFree, s("b")]);
        assert_tokens("a:!b", &[s("a"), Symbol(Colon), Symbol(Bang), s("b")]);
        assert_tokens(
            "a:(b)",
            &[s("a"), Symbol(Colon), StartNested(Paren), s("b"), EndNested],
        );
        assert_tokens(
            "[a]:b",
            &[
                StartNested(Bracket),
                s("a"),
                EndNested,
                Symbol(Colon),
                s("b"),
            ],
        );
        assert_tokens(
            "a |>:b",
            &[s("a"), Symbol(PipeRight), Symbol(Colon), s("b")],
        );
        assert_tokens(
            "a |> :b",
            &[s("a"), Symbol(PipeRight), ColonPriorFree, s("b")],
        );
    }

    #[test]
    fn nested() {
        assert_tokens(
            "a (a b\nc d) {a b\nc d} [a b\nc d] ({a b} c [d e\nf {g (h)}])\n(i)",
            &[
                s("a"),
                StartNested(Paren),
                s("a"),
                s("b"),
                s("c"),
                s("d"),
                EndNested,
                StartNested(Curly),
                s("a"),
                s("b"),
                NextChild,
                s("c"),
                s("d"),
                EndNested,
                StartNested(Bracket),
                s("a"),
                s("b"),
                NextChild,
                s("c"),
                s("d"),
                EndNested,
                StartNested(Paren),
                StartNested(Curly),
                s("a"),
                s("b"),
                EndNested,
                s("c"),
                StartNested(Bracket),
                s("d"),
                s("e"),
                NextChild,
                s("f"),
                StartNested(Curly),
                s("g"),
                StartNested(Paren),
                s("h"),
                EndNested,
                EndNested,
                EndNested,
                EndNested,
                NextChild,
                StartNested(Paren),
                s("i"),
                EndNested,
            ],
        )
    }

    #[test]
    fn doc_comment_expression() {
        assert_tokens(
            "## {{ a }}\nhello",
            &[
                Symbol(DocString(" ".into())),
                StartNested(DocCurly),
                s("a"),
                EndNested,
                Symbol(DocString("".into())),
                NextChild,
                s("hello"),
            ],
        );
    }

    #[test]
    fn doc_comment_expression_mismatch() {
        assert_fail("## {{\nhello");
    }

    #[test]
    fn invalid_separator() {
        assert_fail("(a, b)");
        assert_fail("(a; b)");
    }

    #[test]
    fn nested_mismatch() {
        assert_fail("(");
        assert_fail(")");
        assert_fail("(a {b c\n d [e,f})");
        assert_fail("a {b c\n d [e,f]})");
        assert_fail("## {{");
    }

    fn assert_tokens(s: &str, expected: &[TreeToken]) {
        let toks: Vec<_> = TreeTokens::from(Tokens::from(
            Source::new(StringSource::new("<string>", s.to_owned()))
                .open()
                .unwrap(),
        ))
        .collect::<Result<Vec<_>, _>>()
        .unwrap()
        .into_iter()
        .map(|t| t.unwrap())
        .collect();
        dbg!(&toks);
        assert!(toks == expected);
    }

    fn assert_fail(s: &str) {
        TreeTokens::from(Tokens::from(
            Source::new(StringSource::new("<string>", s.to_owned()))
                .open()
                .unwrap(),
        ))
        .collect::<Result<Vec<_>, _>>()
        .unwrap_err();
    }
}
