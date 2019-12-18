//! The AST definition for script files.

use std::fmt;
use std::io::{BufRead, BufReader, Read};

mod parse;
mod tokenize;

/// A parsed expression.
#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Empty,
    String(String),
    Array(Vec<Expr>),
    SetVariable(String, Box<Expr>),
    UnsetVariable(String),
    Index(Box<Expr>, String),
    Command(Box<Expr>, Vec<Expr>),
    Block(Vec<Expr>),
    Function(Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
}

/// A location in the original (character) input stream.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Location {
    pub start: usize,
    pub length: usize,
}

impl Location {
    /// Create a Location with the given fields.
    pub fn new(start: usize, length: usize) -> Self {
        Location { start, length }
    }

    /// Get the end index of the location.
    pub fn end(&self) -> usize {
        self.start + self.length
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}-{}", self.start, self.start + self.length - 1)
    }
}

impl std::ops::Add for Location {
    type Output = Location;

    fn add(self, other: Location) -> Self::Output {
        &self + &other
    }
}

impl std::ops::Add for &'_ Location {
    type Output = Location;

    fn add(self, other: &Location) -> Self::Output {
        if self.length == 0 {
            other.clone()
        } else if other.length == 0 {
            self.clone()
        } else {
            let start = std::cmp::min(self.start, other.start);
            let end = std::cmp::max(self.end(), other.end());
            Location {
                start,
                length: end - start,
            }
        }
    }
}

impl std::iter::Sum for Location {
    fn sum<I: Iterator<Item = Self>>(mut iter: I) -> Self {
        iter.next()
            .map(|first| iter.fold(first, |a, b| a + b))
            .unwrap_or_default()
    }
}

/// A factory that provides source names and data.
pub trait SourceFactory {
    /// The name of the source.
    fn name(&self) -> String;

    /// Read from the source.
    fn read<'a>(&'a self) -> Result<Box<dyn Read + 'a>, String>;
}

#[derive(Clone, Default)]
struct SourceFactoryRef(Option<std::sync::Arc<dyn SourceFactory + Send + Sync>>);

impl fmt::Debug for SourceFactoryRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.0 {
            None => write!(f, "no SourceFactory"),
            Some(s) => write!(f, "SourceFactory({})", s.name()),
        }
    }
}

impl std::ops::Add for SourceFactoryRef {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        match (&self.0, &other.0) {
            (Some(ref a), Some(ref b)) => {
                if std::sync::Arc::ptr_eq(a, b) {
                    self
                } else {
                    panic!("Cannot combine inequivalent source factories.");
                }
            }
            (Some(_), _) => self,
            (_, Some(_)) => other,
            _ => SourceFactoryRef(None),
        }
    }
}

impl std::iter::Sum for SourceFactoryRef {
    fn sum<I: Iterator<Item = Self>>(mut iter: I) -> Self {
        iter.next()
            .map(|first| iter.fold(first, |a, b| a + b))
            .unwrap_or_default()
    }
}

impl PartialEq for SourceFactoryRef {
    fn eq(&self, other: &Self) -> bool {
        match (&self.0, &other.0) {
            (Some(ref a), Some(ref b)) => std::sync::Arc::ptr_eq(a, b),
            (None, None) => true,
            _ => false,
        }
    }
}

/// A string-based source.
pub struct StringSource(pub String);

impl SourceFactory for StringSource {
    fn name(&self) -> String {
        "<string>".into()
    }

    fn read<'a>(&'a self) -> Result<Box<dyn Read + 'a>, String> {
        let r: &'a [u8] = self.0.as_ref();
        Ok(Box::new(r))
    }
}

/// A file-based source.
pub struct FileSource(pub std::path::PathBuf);

impl SourceFactory for FileSource {
    fn name(&self) -> String {
        format!("{}", self.0.display())
    }

    fn read<'a>(&'a self) -> Result<Box<dyn Read + 'a>, String> {
        Ok(Box::new(
            std::fs::File::open(self.0.clone()).map_err(|e| e.to_string())?,
        ))
    }
}

/// No source.
pub struct NoSource;

impl SourceFactory for NoSource {
    fn name(&self) -> String {
        "none".into()
    }

    fn read<'a>(&'a self) -> Result<Box<dyn Read + 'a>, String> {
        Err("no source".into())
    }
}

/// A type which adds source location to a value.
#[derive(Clone, Debug)]
pub struct Source<T> {
    value: T,
    pub location: Location,
    source: SourceFactoryRef,
}

/// Types which can be converted into a Source<T>.
pub trait IntoSource {
    /// The Output type, which will be present in Source<Output>.
    type Output;

    /// Convert the type into a source.
    fn into_source(self) -> Source<Self::Output>;
}

impl Source<()> {
    /// Create a source with the given factory.
    pub fn new(source: impl SourceFactory + Send + Sync + 'static) -> Self {
        Source {
            value: (),
            location: Location::default(),
            source: SourceFactoryRef(Some(std::sync::Arc::new(source))),
        }
    }

    /// Open a source, returning a Source around the iterator over the source's characters.
    pub fn open(self) -> std::io::Result<Source<impl IntoIterator<Item = char>>> {
        let src = self.source().unwrap();
        let mut r = src
            .read()
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;
        // TODO: inefficient
        let mut s = String::new();
        r.read_to_string(&mut s)?;
        let s: Vec<_> = s.chars().collect();

        Ok(Source {
            value: s,
            location: self.location,
            source: self.source,
        })
    }
}

impl<T> Source<T> {
    /// Convert a source into the inner value.
    pub fn into_value(self) -> T {
        self.value
    }

    /// Get the inner source factory.
    pub fn source(&self) -> Option<std::sync::Arc<dyn SourceFactory + Send + Sync>> {
        self.source.0.clone()
    }

    /// Map the inner value of the source.
    pub fn map<U, F>(self, f: F) -> Source<U>
    where
        F: FnOnce(T) -> U,
    {
        Source {
            value: f(self.value),
            location: self.location,
            source: self.source,
        }
    }

    /// Replace the contents of the source.
    pub fn with<U>(self, u: U) -> Source<U> {
        self.map(|_| u)
    }

    /// Remove a value from a source.
    pub fn take(self) -> (Source<()>, T) {
        (
            Source {
                value: (),
                location: self.location,
                source: self.source,
            },
            self.value,
        )
    }
}

impl<T: PartialEq> Source<T> {
    pub fn total_eq(this: &Self, other: &Self) -> bool {
        this.value == other.value && this.location == other.location && this.source == other.source
    }
}

impl<T: PartialEq> PartialEq for Source<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl<T> std::ops::Deref for Source<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> std::ops::DerefMut for Source<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

impl<T> AsRef<T> for Source<T> {
    fn as_ref(&self) -> &T {
        &self.value
    }
}

impl<T> AsMut<T> for Source<T> {
    fn as_mut(&mut self) -> &mut T {
        &mut self.value
    }
}

impl<T> IntoSource for Source<T> {
    type Output = T;
    fn into_source(self) -> Source<T> {
        self
    }
}

impl<T: IntoSource, U: IntoSource> IntoSource for (T, U) {
    type Output = (Source<T::Output>, Source<U::Output>);

    fn into_source(self) -> Source<Self::Output> {
        (self.0.into_source(), self.1.into_source()).into()
    }
}

impl<T: IntoSource> IntoSource for Vec<T> {
    type Output = Vec<Source<T::Output>>;

    fn into_source(self) -> Source<Self::Output> {
        let (value, rest): (Vec<_>, Vec<_>) = self
            .into_iter()
            .map(|t| {
                let s = t.into_source();
                let source = s.source();
                let loc = s.location.clone();
                (s, (loc, SourceFactoryRef(source)))
            })
            .unzip();

        let (locs, srcs): (Vec<_>, Vec<_>) = rest.into_iter().unzip();
        let location = locs.into_iter().sum();
        let source = srcs.into_iter().sum();

        Source {
            value,
            location,
            source,
        }
    }
}

impl<T: IntoSource> IntoSource for Option<T> {
    type Output = Option<Source<T::Output>>;

    fn into_source(self) -> Source<Self::Output> {
        match self {
            None => Source {
                value: None,
                location: Default::default(),
                source: Default::default(),
            },
            Some(s) => {
                let s = s.into_source();
                let location = s.location.clone();
                let source = s.source.clone();
                Source {
                    value: Some(s),
                    location,
                    source,
                }
            }
        }
    }
}

impl<T, U> std::iter::FromIterator<Source<U>> for Source<T>
where
    T: std::iter::FromIterator<U>,
{
    fn from_iter<I: IntoIterator<Item = Source<U>>>(iter: I) -> Source<T> {
        let (vals, rest): (Vec<_>, Vec<_>) = iter
            .into_iter()
            .map(|s| (s.value, (s.location, s.source)))
            .unzip();
        let (locs, srcs): (Vec<_>, Vec<_>) = rest.into_iter().unzip();
        let value = vals.into_iter().collect();
        let location = locs.into_iter().sum();
        let source = srcs.into_iter().sum();

        Source {
            value,
            location,
            source,
        }
    }
}

impl<T, U> From<(Source<T>, Source<U>)> for Source<(Source<T>, Source<U>)> {
    fn from((t, u): (Source<T>, Source<U>)) -> Self {
        let location = t.location.clone() + u.location.clone();
        let source = t.source.clone() + u.source.clone();
        Source {
            value: (t, u),
            location,
            source,
        }
    }
}

impl<T: fmt::Display + fmt::Debug> fmt::Display for Source<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut start = None;
        let mut end = None;
        let mut startline = None;

        if let Some(source) = &self.source.0 {
            write!(f, "{}", source.name())?;
            match source.read() {
                Ok(reader) => {
                    let mut src = BufReader::new(reader);

                    let mut remaining = self.location.start;
                    let mut linecount = 1;
                    let error = loop {
                        let mut line = String::new();
                        let read = match src.read_line(&mut line) {
                            Ok(s) => s,
                            Err(e) => break Some(e.to_string()),
                        };
                        if read == 0 {
                            break Some("invalid location".to_string());
                        }

                        let chars = line.chars().count();
                        if remaining <= chars {
                            if start.is_none() {
                                start = Some((linecount, remaining + 1));
                                startline = Some(line);
                                remaining += self.location.length;
                                if remaining <= chars {
                                    end = Some((linecount, remaining + 1));
                                    break None;
                                }
                            } else {
                                end = Some((linecount, remaining + 1));
                                break None;
                            }
                        }
                        remaining -= chars;
                        linecount += 1;
                    };

                    match error {
                        Some(e) => write!(f, ": {}\n[error: {}]", &self.value, e),
                        None => {
                            let start = start.unwrap();
                            let end = end.unwrap();
                            let startline = startline.unwrap();
                            let mut underline = String::new();
                            for _ in 1..start.1 {
                                underline.push(' ');
                            }
                            underline.push('^');
                            let endchar = if start.0 == end.0 {
                                end.1
                            } else {
                                startline.chars().count()
                            };
                            for _ in start.1 + 1..endchar {
                                underline.push('-');
                            }
                            write!(
                                f,
                                " ({}:{}-{}:{}): {}\n{}{}",
                                start.0, start.1, end.0, end.1, &self.value, startline, underline
                            )
                        }
                    }
                }
                Err(e) => write!(f, ": {}\n[error reading source: {}]", &self.value, e),
            }
        } else {
            write!(f, "[no source]: {}", &self.value)
        }
    }
}

impl<T: PartialEq> PartialEq<T> for Source<T> {
    fn eq(&self, other: &T) -> bool {
        &self.value == other
    }
}

impl PartialEq<Source<Self>> for tokenize::Token {
    fn eq(&self, other: &Source<Self>) -> bool {
        self == &other.value
    }
}

/// Expressions with source information.
pub type Expr = Source<Expression>;

/// A parsed script.
pub type Script = Vec<Expr>;

/// A script loading error.
pub enum Error {
    Io(std::io::Error),
    /// A tokenization error.
    Tokenize(Source<tokenize::Error>),
    /// A parsing error.
    Parse(Source<pom::Error>),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Io(e) => write!(f, "{}", e),
            Self::Tokenize(e) => write!(f, "{}", e),
            Self::Parse(e) => write!(f, "{}", e),
        }
    }
}

/// Load an AST from the given character stream.
pub fn load(src: Source<()>) -> Result<Script, Error> {
    let toks = tokenize::Tokens::from(src.clone().open().map_err(Error::Io)?)
        .collect::<Result<Vec<_>, _>>()
        .map_err(Error::Tokenize)?;
    let parser = parse::script();
    let parse_error = |e: pom::Error, pos: Option<usize>| {
        pos.and_then(|pos| toks.get(pos))
            .map(|p| p.clone().with(()))
            .unwrap_or(src)
            .with(e)
    };
    parser.parse(&toks).map_err(move |e| {
        Error::Parse(match e {
            pom::Error::Incomplete => parse_error(e, None),
            pom::Error::Mismatch { position, .. } => parse_error(e, Some(position)),
            pom::Error::Conversion { position, .. } => parse_error(e, Some(position)),
            pom::Error::Expect { position, .. } => parse_error(e, Some(position)),
            pom::Error::Custom { position, .. } => parse_error(e, Some(position)),
        })
    })
}
