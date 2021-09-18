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

pub fn stderr(format: crate::options::OutputFormat) -> Option<Box<term::StderrTerminal>> {
    use crate::options::OutputFormat::*;
    let t = term::stderr();
    let is_tty = atty::is(atty::Stream::Stderr);

    if t.is_some() && (format == Pretty || (format == Auto && is_tty)) {
        t
    } else if format == Basic || format == Auto {
        Some(Box::new(TextTerm(std::io::stderr())))
    } else {
        None
    }
}

/// Implement `termcolor::WriteColor` for a `term::Terminal` type.
pub struct TermToTermcolor<T: ?Sized>(pub Box<T>);

fn convert_color(c: &ergo_runtime::error::termcolor::Color, intense: bool) -> term::color::Color {
    use ergo_runtime::error::termcolor::Color;

    if intense {
        match c {
            Color::Black => term::color::BRIGHT_BLACK,
            Color::Blue => term::color::BRIGHT_BLUE,
            Color::Green => term::color::BRIGHT_GREEN,
            Color::Red => term::color::BRIGHT_RED,
            Color::Cyan => term::color::BRIGHT_CYAN,
            Color::Magenta => term::color::BRIGHT_MAGENTA,
            Color::Yellow => term::color::BRIGHT_YELLOW,
            Color::White => term::color::BRIGHT_WHITE,
            _ => term::color::BRIGHT_WHITE,
        }
    } else {
        match c {
            Color::Black => term::color::BLACK,
            Color::Blue => term::color::BLUE,
            Color::Green => term::color::GREEN,
            Color::Red => term::color::RED,
            Color::Cyan => term::color::CYAN,
            Color::Magenta => term::color::MAGENTA,
            Color::Yellow => term::color::YELLOW,
            Color::White => term::color::WHITE,
            _ => term::color::WHITE,
        }
    }
}

fn convert_result(result: term::Result<()>) -> std::io::Result<()> {
    match result {
        Ok(()) => Ok(()),
        Err(e) => match e {
            term::Error::Io(e) => Err(e),
            _ => Ok(()),
        },
    }
}

impl<T> std::io::Write for TermToTermcolor<T>
where
    T: std::io::Write + ?Sized,
{
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.0.flush()
    }
}

impl<T> ergo_runtime::error::termcolor::WriteColor for TermToTermcolor<T>
where
    T: Terminal + ?Sized,
{
    fn supports_color(&self) -> bool {
        self.0.supports_color()
    }

    fn set_color(
        &mut self,
        spec: &ergo_runtime::error::termcolor::ColorSpec,
    ) -> std::io::Result<()> {
        if spec.reset() {
            convert_result(self.0.reset())?;
        }
        if let Some(c) = spec.fg() {
            convert_result(self.0.fg(convert_color(c, spec.intense())))?;
        }
        if let Some(c) = spec.bg() {
            convert_result(self.0.bg(convert_color(c, spec.intense())))?;
        }
        if spec.bold() {
            convert_result(self.0.attr(term::Attr::Bold))?;
        }
        if spec.dimmed() {
            convert_result(self.0.attr(term::Attr::Dim))?;
        }
        if spec.italic() {
            convert_result(self.0.attr(term::Attr::Italic(true)))?;
        }
        if spec.underline() {
            convert_result(self.0.attr(term::Attr::Underline(true)))?;
        }
        Ok(())
    }

    fn reset(&mut self) -> std::io::Result<()> {
        convert_result(self.0.reset())
    }
}

pub struct TextTerm<T>(T);

impl<T: Write> Write for TextTerm<T> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.0.flush()
    }
}

impl<T: Write> Terminal for TextTerm<T> {
    type Output = T;

    fn fg(&mut self, _color: term::color::Color) -> term::Result<()> {
        Ok(())
    }

    fn bg(&mut self, _color: term::color::Color) -> term::Result<()> {
        Ok(())
    }

    fn attr(&mut self, _attr: term::Attr) -> term::Result<()> {
        Ok(())
    }

    fn supports_attr(&self, _attr: term::Attr) -> bool {
        false
    }

    fn reset(&mut self) -> term::Result<()> {
        Ok(())
    }

    fn supports_reset(&self) -> bool {
        false
    }

    fn supports_color(&self) -> bool {
        false
    }

    fn cursor_up(&mut self) -> term::Result<()> {
        Err(term::Error::NotSupported)
    }

    fn delete_line(&mut self) -> term::Result<()> {
        Err(term::Error::NotSupported)
    }

    fn carriage_return(&mut self) -> term::Result<()> {
        Err(term::Error::NotSupported)
    }

    fn get_ref(&self) -> &Self::Output {
        &self.0
    }

    fn get_mut(&mut self) -> &mut Self::Output {
        &mut self.0
    }

    fn into_inner(self) -> Self::Output {
        self.0
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

#[cfg(not(unix))]
struct TerminalInput {}

#[cfg(not(unix))]
impl TerminalInput {
    pub fn new() -> Option<Self> {
        None
    }

    pub fn enable(&mut self) {}

    pub fn disable(&mut self) {}
}

#[cfg(unix)]
struct TerminalInput {
    original_input_settings: termios::Termios,
    current_input_settings: termios::Termios,
}

#[cfg(unix)]
impl TerminalInput {
    pub fn new() -> Option<Self> {
        let fd = std::os::unix::io::AsRawFd::as_raw_fd(&std::io::stdin());
        match termios::Termios::from_fd(fd) {
            Err(_) => None,
            Ok(original_input_settings) => {
                let mut ret = TerminalInput {
                    original_input_settings,
                    current_input_settings: original_input_settings.clone(),
                };
                ret.disable();
                Some(ret)
            }
        }
    }

    pub fn enable(&mut self) {
        let fd = std::os::unix::io::AsRawFd::as_raw_fd(&std::io::stdin());
        self.current_input_settings.c_lflag |=
            termios::ECHO | termios::ECHOE | termios::ECHOK | termios::ECHONL;
        termios::tcsetattr(fd, termios::TCSANOW, &self.current_input_settings)
            .expect("failed to set stdin settings");
    }

    pub fn disable(&mut self) {
        let fd = std::os::unix::io::AsRawFd::as_raw_fd(&std::io::stdin());
        self.current_input_settings.c_lflag &=
            !(termios::ECHO | termios::ECHOE | termios::ECHOK | termios::ECHONL);
        termios::tcsetattr(fd, termios::TCSANOW, &self.current_input_settings)
            .expect("failed to set stdin settings");
    }
}

#[cfg(unix)]
impl Drop for TerminalInput {
    fn drop(&mut self) {
        let fd = std::os::unix::io::AsRawFd::as_raw_fd(&std::io::stdin());
        if let Err(e) = termios::tcsetattr(fd, termios::TCSANOW, &self.original_input_settings) {
            eprintln!("failed to restore stdin settings: {}", e);
        }
    }
}

pub struct TerminalOutput {
    term: Box<term::StdoutTerminal>,
    last_rendered_lines: usize,
    input_settings: Option<TerminalInput>,
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
            input_settings: TerminalInput::new(),
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

    pub fn enable_stdin(&mut self) {
        if let Some(i) = &mut self.input_settings {
            i.enable();
        }
    }

    pub fn disable_stdin(&mut self) {
        if let Some(i) = &mut self.input_settings {
            i.disable();
        }
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
            total += self.term.write(bs)?;
            // Another element in the iterator indicates that this element was the end of a line.
            if iter.peek().is_some() {
                writeln!(self.term)?;
                if self.last_rendered_lines > 0 {
                    self.last_rendered_lines -= 1;
                }
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
        // Render nothing to clear the previous rendered content.
        self.renderer();
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
        Err(term::Error::NotSupported)
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
