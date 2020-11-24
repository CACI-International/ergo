use std::io::Write;
use term::Terminal;

pub mod render {
    pub use super::Render;
    pub use std::io::Write;
    pub use term::Terminal;
}

pub fn stdout(format: crate::options::OutputFormat) -> Option<OutputType> {
    use crate::options::OutputFormat::*;
    let t = term::stdout();
    let is_tty = atty::is(atty::Stream::Stdout);

    if t.is_some() && (format == Pretty || (format == Auto && is_tty)) {
        Some(OutputType::term(t.unwrap()))
    } else if format == Basic || format == Auto {
        Some(OutputType::Dumb(Box::new(std::io::stdout())))
    } else {
        None
    }
}

/// A type that supports rendering to a terminal.
pub trait Render {
    /// Render output to the terminal.
    ///
    /// Implementations should write to the terminal as if it were completely blank and no
    /// attributes were set (there is no need to clear lines or position the cursor).
    fn render<Target: Write + Terminal>(&self, to: &mut Target) -> std::io::Result<()>;
}

pub struct TerminalOutput {
    term: Box<term::StdoutTerminal>,
    last_rendered_lines: usize,
}

pub struct Renderer<'a>(RendererInner<'a>);

struct RendererInner<'a> {
    term: &'a mut TerminalOutput,
    lines: usize,
}

pub enum OutputType {
    Term(TerminalOutput),
    Dumb(Box<dyn Write + Send>),
}

impl OutputType {
    fn term(term: Box<term::StdoutTerminal>) -> Self {
        OutputType::Term(TerminalOutput {
            term,
            last_rendered_lines: 0,
        })
    }
}

impl<T: Render> std::ops::AddAssign<&T> for Renderer<'_> {
    fn add_assign(&mut self, rhs: &T) {
        rhs.render(&mut self.0).expect("failed to render");
        self.0.reset().expect("failed to reset terminal");
    }
}

impl<'a> Renderer<'a> {
    fn new(term: &'a mut TerminalOutput) -> Self {
        Renderer(RendererInner { term, lines: 0 })
    }
}

impl TerminalOutput {
    pub fn renderer(&mut self) -> Renderer {
        self.term.reset().expect("failed to reset terminal");
        // Move cursor to beginning of previously-rendered output.
        for _ in 0..self.last_rendered_lines {
            self.term.cursor_up().expect("failed to write to terminal");
        }
        Renderer::new(self)
    }

    pub fn renderer_after<T: std::fmt::Display>(&mut self, to_write: T) -> Renderer {
        self.term.reset().expect("failed to reset terminal");
        // Move cursor to beginning of previously-rendered output.
        for _ in 0..self.last_rendered_lines {
            self.term.cursor_up().expect("failed to write to terminal");
        }
        writeln!(self, "{}", to_write).expect("failed to write");
        Renderer::new(self)
    }

    /// Write the given bytes, clearing lines of any previous characters.
    ///
    /// Returns the number of bytes and newlines that were written.
    fn write_clearing(&mut self, bytes: &[u8]) -> std::io::Result<(usize, usize)> {
        let mut total = 0;
        let mut nls = 0;
        let mut iter = bytes.split(|v| char::from(*v) == '\n').peekable();
        while let Some(bs) = iter.next() {
            self.term.delete_line()?;
            if self.last_rendered_lines > 0 {
                self.last_rendered_lines -= 1;
            }
            total += self.term.write(bs)?;
            if iter.peek().is_some() {
                writeln!(self.term)?;
                nls += 1;
            }
        }
        Ok((total + nls, nls))
    }
}

impl Write for TerminalOutput {
    fn write(&mut self, bytes: &[u8]) -> std::io::Result<usize> {
        self.write_clearing(bytes).map(|(a, _)| a)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.term.flush()
    }
}

impl Drop for TerminalOutput {
    fn drop(&mut self) {
        let lines = self.last_rendered_lines;
        let mut renderer = self.renderer();
        for _ in 0..lines {
            writeln!(renderer.0).expect("failed to write to terminal");
        }
    }
}

impl Write for RendererInner<'_> {
    fn write(&mut self, bytes: &[u8]) -> std::io::Result<usize> {
        let (ret, lines) = self.term.write_clearing(bytes)?;
        self.lines += lines;
        Ok(ret)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.term.flush()
    }
}

macro_rules! forward {
    ( $name:ident ( &mut self, $( $arg:ident : $argt:ty ),* ) -> $ret:ty) => (
        fn $name(&mut self, $( $arg: $argt ),* ) -> $ret {
            self.term.term.$name($( $arg ),*)
        }
    );
    ( $name:ident ( &self, $( $arg:ident : $argt:ty ),* ) -> $ret:ty) => (
        fn $name(&self, $( $arg: $argt ),* ) -> $ret {
            self.term.term.$name($( $arg ),*)
        }
    );
}

impl Terminal for RendererInner<'_> {
    type Output = Self;

    forward!(fg(&mut self, color: term::color::Color) -> term::Result<()>);
    forward!(bg(&mut self, color: term::color::Color) -> term::Result<()>);
    forward!(attr(&mut self, attr: term::Attr) -> term::Result<()>);
    forward!(supports_attr(&self, attr: term::Attr) -> bool);
    forward!(reset(&mut self,) -> term::Result<()>);
    forward!(supports_reset(&self,) -> bool);
    forward!(supports_color(&self,) -> bool);
    forward!(delete_line(&mut self,) -> term::Result<()>);
    forward!(carriage_return(&mut self,) -> term::Result<()>);

    fn cursor_up(&mut self) -> term::Result<()> {
        return Err(term::Error::NotSupported);
    }

    fn get_ref(&self) -> &Self::Output {
        self
    }

    fn get_mut(&mut self) -> &mut Self::Output {
        self
    }

    fn into_inner(self) -> Self::Output {
        self
    }
}

impl Drop for RendererInner<'_> {
    fn drop(&mut self) {
        let lines = self.lines;
        let mut lines_up = 0;
        // Clear any following lines from prior renders.
        while self.term.last_rendered_lines > 0 {
            writeln!(self).expect("failed to write to terminal");
            lines_up += 1;
        }
        // Move the cursor back to the line after our last output line.
        for _ in 0..lines_up {
            self.term
                .term
                .cursor_up()
                .expect("failed to write to terminal");
        }
        self.carriage_return().expect("failed to write to terminal");
        self.term.last_rendered_lines = lines;
        // Flush to ensure any interleaving stderr will end up in the correct place and with the
        // correct settings.
        self.flush().expect("flush failed");
    }
}
