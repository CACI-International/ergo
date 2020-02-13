use std::io::Write;
use term::Terminal;

pub fn stdout() -> OutputInterface {
    match (term::stdout(), atty::is(atty::Stream::Stdout)) {
        (Some(v), true) => OutputInterface::Term(v),
        _ => OutputInterface::Dumb(Box::new(std::io::stdout())),
    }
}

pub enum OutputInterface {
    Term(Box<term::StdoutTerminal>),
    Dumb(Box<dyn Write + Send>),
}

impl OutputInterface {
    pub fn is_tty(&self) -> bool {
        match self {
            OutputInterface::Term(_) => true,
            OutputInterface::Dumb(_) => false,
        }
    }
}

impl Write for OutputInterface {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match self {
            OutputInterface::Term(v) => v.write(buf),
            OutputInterface::Dumb(v) => v.write(buf),
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        match self {
            OutputInterface::Term(v) => v.flush(),
            OutputInterface::Dumb(v) => v.flush(),
        }
    }
}

macro_rules! forward {
    ( $name:ident ( &mut self, $( $arg:ident : $argt:ty ),* ) -> $ret:ty, $fallback:expr ) => (
        fn $name(&mut self, $( $arg: $argt ),* ) -> $ret {
            match self {
                OutputInterface::Term(v) => v.$name($( $arg ),*),
                OutputInterface::Dumb(_) => $fallback
            }
        }
    );
    ( $name:ident ( &self, $( $arg:ident : $argt:ty ),* ) -> $ret:ty, $fallback:expr ) => (
        fn $name(&self, $( $arg: $argt ),* ) -> $ret {
            match self {
                OutputInterface::Term(v) => v.$name($( $arg ),*),
                OutputInterface::Dumb(_) => $fallback
            }
        }
    );
}

impl Terminal for OutputInterface {
    type Output = Self;

    forward!(fg(&mut self, color: term::color::Color) -> term::Result<()>, Ok(()));
    forward!(bg(&mut self, color: term::color::Color) -> term::Result<()>, Ok(()));
    forward!(attr(&mut self, attr: term::Attr) -> term::Result<()>, Ok(()));
    forward!(supports_attr(&self, attr: term::Attr) -> bool, false);
    forward!(reset(&mut self,) -> term::Result<()>, Ok(()));
    forward!(supports_reset(&self,) -> bool, true);
    forward!(supports_color(&self,) -> bool, false);
    forward!(cursor_up(&mut self,) -> term::Result<()>, Ok(()));
    forward!(delete_line(&mut self,) -> term::Result<()>, Ok(()));
    forward!(carriage_return(&mut self,) -> term::Result<()>, Ok(()));

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
