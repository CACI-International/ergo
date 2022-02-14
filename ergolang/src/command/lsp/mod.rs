use super::format::Formatter;
use ergo_runtime::source::{Location, Source};
use ergo_script::ast::tokenize;
use lspower::jsonrpc::Result;
use lspower::lsp::*;
use lspower::{Client, LanguageServer, LspService, Server};

mod files;

use files::Files;

#[derive(Debug, clap::Args)]
/// Run a language server.
pub struct Lsp {}

impl super::Command for Lsp {
    fn run(self) -> std::result::Result<(), String> {
        let stdin = tokio::io::stdin();
        let stdout = tokio::io::stdout();

        let (service, messages) = LspService::new(|client| Service {
            _client: client,
            files: Default::default(),
        });
        tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .worker_threads(2)
            .build()
            .unwrap()
            .block_on(
                Server::new(stdin, stdout)
                    .interleave(messages)
                    .serve(service),
            );
        Ok(())
    }
}

#[derive(Debug)]
struct Service {
    _client: Client,
    files: Files,
}

struct TokenResults {
    tokens: Vec<SemanticToken>,
    line_lengths: Vec<u32>,
    last_start: usize,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct TokenType(u32);

impl TokenType {
    pub const MACRO: Self = TokenType(0);
    pub const COMMENT: Self = TokenType(1);
    pub const STRING: Self = TokenType(2);
    pub const OPERATOR: Self = TokenType(3);
    pub const FUNCTION: Self = TokenType(4);

    pub fn register() -> Vec<SemanticTokenType> {
        vec![
            SemanticTokenType::MACRO,
            SemanticTokenType::COMMENT,
            SemanticTokenType::STRING,
            SemanticTokenType::OPERATOR,
            SemanticTokenType::FUNCTION,
        ]
    }
}

#[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct TokenModifier(u32);

impl TokenModifier {
    pub const DOC: Self = TokenModifier(1 << 0);

    pub fn register() -> Vec<SemanticTokenModifier> {
        vec![SemanticTokenModifier::DOCUMENTATION]
    }
}

impl std::ops::BitOr for TokenModifier {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self {
        TokenModifier(self.0 | rhs.0)
    }
}

fn subswap(a: &mut usize, mut b: usize) -> u32 {
    std::mem::swap(a, &mut b);
    (*a - b) as u32
}

impl TokenResults {
    pub fn new(source: &ropey::Rope) -> Self {
        let mut line_lengths = source
            .lines()
            // Exclude final newline in line lengths
            .map(|line| (line.len_bytes() - '\n'.len_utf8()) as u32)
            .collect::<Vec<_>>();
        // Reverse so popping can be done efficiently
        line_lengths.reverse();
        TokenResults {
            tokens: Default::default(),
            line_lengths,
            last_start: 0,
        }
    }

    pub fn into_inner(self) -> Vec<SemanticToken> {
        self.tokens
    }

    pub fn push(&mut self, location: Location, tp: TokenType, modifiers: TokenModifier) {
        let mut delta_start: u32 = subswap(&mut self.last_start, location.start);
        let mut delta_line: u32 = 0;
        while let Some(l) = self.line_lengths.last_mut() {
            if delta_start <= *l {
                *l -= delta_start;
                break;
            } else {
                // +1 for newline
                delta_start -= *l + 1;
                delta_line += 1;
                self.line_lengths.pop();
            }
        }
        self.tokens.push(dbg!(SemanticToken {
            delta_line,
            delta_start,
            length: location.length as u32,
            token_type: tp.0,
            token_modifiers_bitset: modifiers.0,
        }));
    }
}

fn char_offset(s: &ropey::Rope, pos: Position) -> Option<usize> {
    let line_offset = s.try_line_to_char(pos.line as usize).ok()?;
    Some(line_offset + pos.character as usize)
}

#[derive(Debug, Clone)]
struct RopeSlice<'a>(ropey::RopeSlice<'a>);

impl<'a> From<RopeSlice<'a>> for std::borrow::Cow<'a, str> {
    fn from(s: RopeSlice<'a>) -> Self {
        s.0.into()
    }
}

impl<'a> From<ropey::RopeSlice<'a>> for RopeSlice<'a> {
    fn from(s: ropey::RopeSlice<'a>) -> Self {
        RopeSlice(s)
    }
}

impl<'a> std::ops::Deref for RopeSlice<'a> {
    type Target = ropey::RopeSlice<'a>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> tokenize::StringSlice<'a> for RopeSlice<'a> {
    fn next_char(&mut self) -> Option<char> {
        let c = self.0.get_char(0)?;
        self.0 = self.0.slice(1..);
        Some(c)
    }

    fn peek_char(&self) -> Option<char> {
        self.0.get_char(0)
    }

    fn split_at(&self, byte_index: usize) -> (Self, Self) {
        let c = self.0.byte_to_char(byte_index);
        (self.0.slice(..c).into(), self.0.slice(c..).into())
    }

    fn len(&self) -> usize {
        self.0.len_bytes()
    }

    fn ends_with_char(&self, c: char) -> bool {
        self.0.len_chars() > 0 && self.0.char(self.0.len_chars() - 1) == c
    }

    unsafe fn slice_to(&self, byte_index: usize) -> Self {
        let c = self.0.byte_to_char(byte_index);
        self.0.slice(..c).into()
    }

    fn from_str(s: &'static str) -> Self {
        RopeSlice(s.into())
    }

    fn write_to_string(slices: &[Self], s: &mut String) {
        let total = slices.iter().map(|s| s.0.len_bytes()).sum();
        s.reserve(total);
        for slice in slices {
            for chunk in slice.0.chunks() {
                s.push_str(chunk);
            }
        }
    }
}

#[lspower::async_trait]
impl LanguageServer for Service {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        let mut capabilities = ServerCapabilities::default();
        capabilities.semantic_tokens_provider = Some(
            SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
                work_done_progress_options: Default::default(),
                range: Some(false),
                full: Some(SemanticTokensFullOptions::Bool(true)),
                legend: SemanticTokensLegend {
                    token_types: TokenType::register(),
                    token_modifiers: TokenModifier::register(),
                },
            }),
        );
        capabilities.text_document_sync = Some(TextDocumentSyncCapability::Kind(
            TextDocumentSyncKind::INCREMENTAL,
        ));
        capabilities.document_formatting_provider = Some(OneOf::Left(true));
        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: env!("CARGO_PKG_NAME").into(),
                version: Some(env!("CARGO_PKG_VERSION").into()),
            }),
            capabilities,
        })
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let mut source = self.files.content_mut(&params.text_document.uri).await;
        for change in params.content_changes {
            if let Some(r) = change.range {
                match (char_offset(&*source, r.start), char_offset(&*source, r.end)) {
                    (Some(start_char), Some(end_char)) => {
                        if source.try_remove(start_char..end_char).is_ok() {
                            source.insert(start_char, change.text.as_str());
                        }
                    }
                    // XXX send error
                    _ => (),
                }
            } else {
                *source = change.text.into();
            }
        }
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let source = self.files.content(&params.text_document.uri).await;

        let formatter = Formatter { line_width: 100 };

        match formatter.format(
            tokenize::Tokens::from(Source::new(0).with(RopeSlice(source.slice(..))))
                .map(|r| r.map(Source::unwrap).map_err(Source::unwrap)),
        ) {
            Err(_) => {
                // XXX send error
                Ok(None)
            }
            Ok(new_text) => Ok(Some(vec![TextEdit {
                range: Range {
                    start: Position {
                        line: 0,
                        character: 0,
                    },
                    end: Position {
                        line: source.len_lines() as u32,
                        character: source.line(source.len_lines() - 1).len_chars() as u32,
                    },
                },
                new_text,
            }])),
        }
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        use tokenize::{LeaderToken, PairedToken, SymbolicToken, Token};

        let source = self.files.content(&params.text_document.uri).await;
        let mut results = TokenResults::new(&*source);
        let mut paired_toks = Vec::new();

        enum CommentNext {
            Off,
            On,
            Paired(usize),
        }

        let mut comment_next = CommentNext::Off;

        for t in tokenize::Tokens::from(Source::new(0).with(RopeSlice(source.slice(..)))) {
            use LeaderToken as L;
            use PairedToken as P;
            use SymbolicToken::*;
            let tok = match t {
                Err(_) => continue, // Report diagnostic?
                Ok(t) => t,
            };

            match comment_next {
                CommentNext::Off => match tok.value() {
                    Token::String(s) => match paired_toks.last() {
                        Some(P::Quote | P::Apostrophe) => {
                            results.push(tok.location, TokenType::STRING, Default::default())
                        }
                        Some(P::Hash) => {
                            results.push(tok.location, TokenType::COMMENT, Default::default())
                        }
                        Some(P::DoubleHash) => {
                            results.push(tok.location, TokenType::COMMENT, TokenModifier::DOC)
                        }
                        _ => {
                            if [
                                "ergo",
                                "std",
                                "workspace",
                                "fn",
                                "pat",
                                "index",
                                "bind",
                                "unset",
                                "doc",
                            ]
                            .into_iter()
                            .any(|k| **s == k)
                            {
                                results.push(tok.location, TokenType::FUNCTION, Default::default());
                            }
                        }
                    },
                    Token::Symbol(Equal | Arrow | Colon) => {
                        results.push(tok.location, TokenType::OPERATOR, Default::default())
                    }
                    Token::Symbol(Caret | ColonPrefix | Pipe | PipeLeft | PipeRight) => {
                        results.push(tok.location, TokenType::MACRO, Default::default())
                    }
                    Token::Symbol(Hash) => {
                        comment_next = CommentNext::On;
                        results.push(tok.location, TokenType::COMMENT, Default::default())
                    }
                    Token::Symbol(DoubleHash) => {
                        results.push(tok.location, TokenType::COMMENT, TokenModifier::DOC)
                    }
                    Token::Leader(L::Apostrophe) => {
                        results.push(tok.location, TokenType::STRING, Default::default())
                    }
                    Token::Leader(L::Hash) => {
                        results.push(tok.location, TokenType::COMMENT, Default::default())
                    }
                    Token::Leader(L::DoubleHash) => {
                        results.push(tok.location, TokenType::COMMENT, TokenModifier::DOC)
                    }
                    Token::StartNested(P::Quote | P::Apostrophe) => {
                        results.push(tok.location, TokenType::STRING, Default::default())
                    }
                    Token::StartNested(P::Hash) => {
                        results.push(tok.location, TokenType::COMMENT, Default::default())
                    }
                    Token::StartNested(P::DoubleHash) => {
                        results.push(tok.location, TokenType::COMMENT, TokenModifier::DOC)
                    }
                    Token::EndNested => match paired_toks.last() {
                        Some(P::Quote) => {
                            results.push(tok.location, TokenType::STRING, Default::default())
                        }
                        _ => (),
                    },
                    _ => (),
                },
                CommentNext::On => {
                    if let Token::StartNested(_) = tok.value() {
                        comment_next = CommentNext::Paired(paired_toks.len());
                    } else {
                        comment_next = CommentNext::Off;
                    }
                    results.push(tok.location, TokenType::COMMENT, Default::default())
                }
                CommentNext::Paired(n) => {
                    if let Token::EndNested = tok.value() {
                        if paired_toks.len() == n + 1 {
                            comment_next = CommentNext::Off;
                        }
                    }
                    results.push(tok.location, TokenType::COMMENT, Default::default())
                }
            }

            // Always maintain paired tokens
            match tok.value() {
                Token::StartNested(p) => {
                    paired_toks.push(p.clone());
                }
                Token::EndNested => {
                    paired_toks.pop();
                }
                _ => (),
            }
        }
        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: results.into_inner(),
        })))
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}
