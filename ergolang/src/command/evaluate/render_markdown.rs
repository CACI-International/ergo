use pretty::{
    termcolor::{self, Color, ColorSpec},
    DocAllocator,
};
use pulldown_cmark::{CowStr, Parser};

#[derive(Clone, Copy)]
pub struct ColorSupport(Option<supports_color::ColorLevel>);

impl ColorSupport {
    pub fn new() -> Self {
        ColorSupport(supports_color::on(supports_color::Stream::Stdout))
    }
}

/// Render a markdown string nicely to the terminal.
pub fn render_markdown(color_support: ColorSupport, markdown: &str) {
    let renderer = DocRenderer::default();
    let doc = renderer.render(color_support, markdown);
    if let Err(e) = doc.render_colored(
        terminal_size::terminal_size()
            .map(|(w, _)| w.0 as usize)
            .unwrap_or(100),
        termcolor::BufferedStandardStream::stdout(termcolor::ColorChoice::Auto),
    ) {
        log::error!("{}", e);
    }
}

type Arena<'a> = pretty::Arena<'a, ColorSpec>;
type DocBuilder<'a> = pretty::DocBuilder<'a, Arena<'a>, ColorSpec>;

#[derive(Default)]
struct DocRenderer<'a> {
    arena: Arena<'a>,
}

#[derive(Default, Clone, Copy)]
struct Settings {
    codeblock: bool,
}

struct Styles {
    bold: ColorSpec,
    italic: ColorSpec,
    strikethrough: ColorSpec,
    code: ColorSpec,
    link: ColorSpec,
    h1: ColorSpec,
    h2: ColorSpec,
    h3: ColorSpec,
    h4: ColorSpec,
    h5: ColorSpec,
    h6: ColorSpec,
}

impl Styles {
    pub fn new(_color_support: ColorSupport) -> Self {
        let mut bold = ColorSpec::new();
        bold.set_bold(true);

        let mut italic = ColorSpec::new();
        italic.set_italic(true);

        let mut strikethrough = ColorSpec::new();
        strikethrough.set_dimmed(true).set_fg(Some(Color::Red));

        let mut code = ColorSpec::new();
        code.set_bg(Some(Color::Black))
            .set_fg(Some(Color::Cyan))
            .set_reset(false);

        let mut link = ColorSpec::new();
        link.set_underline(true).set_fg(Some(Color::Blue));

        let mut h1 = ColorSpec::new();
        h1.set_underline(true).set_bold(true);

        let mut h2 = ColorSpec::new();
        h2.set_underline(true).set_bold(true).set_dimmed(true);

        let mut h3 = ColorSpec::new();
        h3.set_underline(true);

        let mut h4 = ColorSpec::new();
        h4.set_underline(true).set_dimmed(true);

        let mut h5 = ColorSpec::new();
        h5.set_fg(Some(Color::Yellow));

        let mut h6 = ColorSpec::new();
        h6.set_fg(Some(Color::Yellow)).set_dimmed(true);

        Styles {
            bold,
            italic,
            strikethrough,
            code,
            link,
            h1,
            h2,
            h3,
            h4,
            h5,
            h6,
        }
    }
}

#[derive(Clone, Copy)]
struct DocRendererState<'a, 'content> {
    arena: &'content Arena<'content>,
    styles: &'a Styles,
    settings: Settings,
}

impl<'a> DocRenderer<'a> {
    pub fn render(&'a self, color_support: ColorSupport, markdown: &'a str) -> DocBuilder<'a> {
        let styles = Styles::new(color_support);
        let mut events = Parser::new(markdown);
        DocRendererState {
            arena: &self.arena,
            styles: &styles,
            settings: Default::default(),
        }
        .render(&mut events)
    }
}

impl<'a, 'content> DocRendererState<'a, 'content> {
    fn render(self, events: &mut Parser<'content, '_>) -> DocBuilder<'content> {
        let mut doc = self.arena.nil();
        if let Some(b) = self.render_one(events, true) {
            doc += b;
        }
        while let Some(b) = self.render_one(events, false) {
            doc += b;
        }
        doc
    }

    fn codeblock(mut self) -> Self {
        self.settings.codeblock = true;
        self
    }

    fn break_text<Split, F>(
        self,
        text: CowStr<'content>,
        split: Split,
        then: F,
    ) -> DocBuilder<'content>
    where
        Split: FnMut(char) -> bool,
        F: FnOnce(Vec<std::borrow::Cow<'content, str>>) -> DocBuilder<'content>,
    {
        match text {
            CowStr::Borrowed(s) => {
                then(s.split(split).map(|s| std::borrow::Cow::from(s)).collect())
            }
            CowStr::Boxed(s) => then(
                s.split(split)
                    .map(|s| std::borrow::Cow::from(s.to_owned()))
                    .collect(),
            ),
            CowStr::Inlined(s) => then(
                s.split(split)
                    .map(|s| std::borrow::Cow::from(s.to_owned()))
                    .collect(),
            ),
        }
    }

    fn replace_text<Split>(
        self,
        text: CowStr<'content>,
        split: Split,
        replace: DocBuilder<'content>,
    ) -> DocBuilder<'content>
    where
        Split: FnMut(char) -> bool,
    {
        match text {
            CowStr::Borrowed(s) => self
                .arena
                .intersperse(s.split(split).map(|s| self.arena.text(s)), replace),
            CowStr::Boxed(s) => self.arena.intersperse(
                s.split(split).map(|s| self.arena.text(s.to_owned())),
                replace,
            ),
            CowStr::Inlined(s) => self.arena.intersperse(
                s.split(split).map(|s| self.arena.text(s.to_owned())),
                replace,
            ),
        }
    }

    fn render_one(
        self,
        events: &mut Parser<'content, '_>,
        first: bool,
    ) -> Option<DocBuilder<'content>> {
        let e = events.next()?;

        let sep = if first {
            self.arena.nil()
        } else {
            self.arena.hardline()
        };
        let line_sep = sep.clone() + sep.clone();

        use pulldown_cmark::{Event::*, HeadingLevel::*, Tag::*};
        Some(match e {
            Start(tag) => match tag {
                Paragraph => line_sep + self.render(events),
                Heading(level, _, _) => {
                    line_sep
                        + self.render(events).annotate(match level {
                            H1 => self.styles.h1.clone(),
                            H2 => self.styles.h2.clone(),
                            H3 => self.styles.h3.clone(),
                            H4 => self.styles.h4.clone(),
                            H5 => self.styles.h5.clone(),
                            H6 => self.styles.h6.clone(),
                        })
                }
                BlockQuote => {
                    sep + self
                        .render(events)
                        .indent(4)
                        .annotate(self.styles.italic.clone())
                }
                CodeBlock(_) => sep + self.codeblock().render(events),
                List(ordered) => {
                    let mut items = Vec::new();
                    match ordered {
                        Some(mut n) => {
                            while let Some(Start(Item)) = events.next() {
                                let item = self.render(events);
                                items.push(
                                    self.arena
                                        .as_string(n)
                                        .append(". ")
                                        .annotate(self.styles.bold.clone())
                                        .append(item.hang(0)),
                                );
                                n += 1;
                            }
                        }
                        None => {
                            while let Some(Start(Item)) = events.next() {
                                let item = self.render(events);
                                items.push(
                                    self.arena
                                        .text("\u{2022} ")
                                        .annotate(self.styles.bold.clone())
                                        .append(item.hang(0)),
                                );
                            }
                        }
                    }
                    sep + self
                        .arena
                        .intersperse(items.into_iter(), self.arena.hardline())
                        .indent(2)
                }
                Item => self.render(events),
                FootnoteDefinition(s) => {
                    sep + self.arena.text(s).braces() + ": " + self.render(events)
                }
                Table(_) | TableHead | TableRow | TableCell => line_sep + self.render(events),
                Emphasis => self.render(events).annotate(self.styles.italic.clone()),
                Strong => self.render(events).annotate(self.styles.bold.clone()),
                Strikethrough => self
                    .render(events)
                    .annotate(self.styles.strikethrough.clone()),
                Link(_, url, _) | Image(_, url, _) => {
                    self.render(events).braces()
                        + self
                            .arena
                            .text(url)
                            .annotate(self.styles.link.clone())
                            .parens()
                }
            },
            End(_) => return None,
            Text(text) => {
                if self.settings.codeblock {
                    self.break_text(
                        text,
                        |c| c == '\n',
                        |lines| {
                            let line_length = lines.iter().map(|l| l.len()).max().unwrap_or(0);
                            let drop_last = lines.last().map(|l| l.is_empty()).unwrap_or(false);
                            let line_count = lines.len() - if drop_last { 1 } else { 0 };
                            self.arena.intersperse(
                                lines.into_iter().take(line_count).map(|l| {
                                    let spaces = line_length - l.len();
                                    // There is a bug where applying the style to the whole
                                    // block (across hardlines) only applies it to the first
                                    // line, so we apply it here to each line.
                                    (self.arena.text(l) + self.arena.text(" ".repeat(spaces)))
                                        .annotate(self.styles.code.clone())
                                }),
                                self.arena.hardline(),
                            )
                        },
                    )
                } else {
                    self.replace_text(text, char::is_whitespace, self.arena.softline())
                }
            }
            Code(code) => self.arena.text(code).annotate(self.styles.code.clone()),
            Html(html) => self.arena.text(html),
            FootnoteReference(r) => self.arena.text(r).braces(),
            SoftBreak => self.arena.softline(),
            HardBreak => self.arena.hardline(),
            Rule => line_sep + self.arena.text("--------------------------------"),
            TaskListMarker(_) => {
                log::error!("unexpected task list marker event from markdown parser");
                self.arena.nil()
            }
        })
    }
}
