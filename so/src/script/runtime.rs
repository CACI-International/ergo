//! Script runtime definitions.

use super::ast::{
    ArrayPattern, CmdPat, Expression, IntoSource, MapPattern, MergeExpression, Pat, Pattern, Source,
};
use crate::constants::LOAD_PATH_BINDING;
use grease::{depends, match_value, IntoValue, Plan, Value};
use log::trace;
use std::collections::BTreeMap;
use std::fmt;
use std::iter::FromIterator;
use std::path::PathBuf;
use std::str::FromStr;

pub mod cache;
pub mod debug;
pub mod exec;
pub mod fold;
pub mod has;
pub mod load;
pub mod map;
pub mod path;
pub mod seq;
pub mod track;
pub mod variable;

pub mod script_types {
    use super::{CmdPat, Error, Eval, Expression, Source};
    use grease::{depends, GetValueType, TypedValue, Value};
    use std::collections::BTreeMap;
    use std::fmt;

    /// Script unit type.
    pub type ScriptUnit = ();

    /// Script string type.
    pub type ScriptString = String;

    /// Script array type.
    #[derive(Clone, Debug, GetValueType, PartialEq)]
    pub struct ScriptArray(pub Vec<Source<Value>>);

    impl From<ScriptArray> for TypedValue<ScriptArray> {
        fn from(v: ScriptArray) -> Self {
            let deps = depends![join v.0.iter().map(|s| &**s)];
            Self::constant_deps(v, deps)
        }
    }

    /// Script map type.
    #[derive(Clone, Debug, GetValueType, PartialEq)]
    pub struct ScriptMap(pub BTreeMap<String, Source<Value>>);

    impl From<ScriptMap> for TypedValue<ScriptMap> {
        fn from(v: ScriptMap) -> Self {
            let deps =
                v.0.iter()
                    .map(|(a, v)| depends![*a, **v])
                    .flatten()
                    .collect::<Vec<_>>();
            Self::constant_deps(v, deps)
        }
    }

    pub type Env = BTreeMap<String, Eval<Source<Value>>>;

    /// Script function type.
    #[derive(GetValueType)]
    pub enum ScriptFunction {
        UserFunction(CmdPat, Source<Expression>, Env),
        BuiltinFunction(Box<Builtin>),
    }

    type Builtin = dyn Fn(&mut grease::Context<super::FunctionContext>) -> Result<Eval<Value>, Error>
        + Send
        + Sync;

    impl std::hash::Hash for ScriptFunction {
        fn hash<H: std::hash::Hasher>(&self, h: &mut H) {
            match self {
                Self::UserFunction(p, s, _) => {
                    p.hash(h);
                    s.hash(h);
                }
                Self::BuiltinFunction(b) => std::ptr::hash(b, h),
            }
        }
    }

    impl fmt::Debug for ScriptFunction {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                Self::UserFunction(p, e, _) => write!(f, "UserFunction: {:?} -> {:?}", p, e),
                Self::BuiltinFunction(_) => write!(f, "BuiltinFunction"),
            }
        }
    }
}

pub mod builtin_function_prelude {
    pub use super::script_types::*;
    pub use super::{
        Error, Eval, FunctionArguments, FunctionContext, LoadScript, Runtime, SourceContext,
    };
    pub use grease::{Context, Value};

    #[macro_export]
    macro_rules! def_builtin {
        ( $ctx:ident => $body:expr ) => {
            pub fn builtin() -> ::grease::Value {
                $crate::script::runtime::ScriptFunction::BuiltinFunction(Box::new(builtin_impl))
                    .into()
            }

            fn builtin_impl($ctx: &mut Context<FunctionContext>) -> Result<Eval<Value>, Error> {
                $body
            }
        };
    }

    #[macro_export]
    macro_rules! eval_error {
        ($ctx:expr , $val:expr) => {
            $crate::eval_error!($ctx, $val, {
                return Ok(Eval::Error);
            })
        };
        ($ctx:expr , $val:expr , $then:expr) => {
            match $val {
                Err(e) => {
                    $ctx.error(e);
                    $then
                }
                Ok(v) => v,
            }
        };
    }

    #[macro_export]
    macro_rules! script_value_as {
        ($ctx:expr , $val:expr , $ty:ty , $err:expr) => {
            $crate::eval_error!(
                $ctx,
                $val.map(|v| {
                    v.typed::<$ty>()
                        .map_err(|_| $err.into())
                        .and_then(|v| v.get())
                })
                .transpose_err()
            )
        };
    }

    pub use crate::def_builtin;
    pub use crate::eval_error;
    pub use crate::script_value_as;
}

use builtin_function_prelude::eval_error;
use builtin_function_prelude::script_value_as;
use script_types::*;

/// The script context data.
pub struct Runtime {
    env: Vec<Env>,
    errors: Vec<SourceContext<Error>>,
    working_dir: PathBuf,
    load_cache: BTreeMap<PathBuf, Eval<Source<Value>>>,
}

impl Default for Runtime {
    fn default() -> Self {
        Runtime {
            // The default context should start with an empty map as the base.
            env: vec![Default::default()],
            errors: vec![],
            working_dir: std::env::current_dir().unwrap(),
            load_cache: Default::default(),
        }
    }
}

impl Runtime {
    pub fn env_insert<T: Into<Eval<Source<Value>>>>(&mut self, k: String, v: T) {
        self.env.last_mut().unwrap().insert(k, v.into());
    }

    pub fn env_extend<T: IntoIterator<Item = (String, Eval<Source<Value>>)>>(&mut self, v: T) {
        self.env.last_mut().unwrap().extend(v);
    }

    pub fn env_remove<Q: ?Sized>(&mut self, k: &Q) -> Option<Eval<Source<Value>>>
    where
        String: std::borrow::Borrow<Q>,
        Q: Ord,
    {
        self.env.last_mut().unwrap().remove(k)
    }

    pub fn env_get<Q: ?Sized>(&self, k: &Q) -> Option<&Eval<Source<Value>>>
    where
        String: std::borrow::Borrow<Q>,
        Q: Ord,
    {
        self.env.iter().rev().find_map(|m| m.get(k))
    }

    pub fn env_flatten(&self) -> BTreeMap<String, Eval<Source<Value>>> {
        self.env.iter().fold(Default::default(), |mut e, m| {
            e.append(&mut m.clone());
            e
        })
    }

    pub fn error<E: Into<SourceContext<Error>>>(&mut self, err: E) {
        self.errors.push(err.into());
    }

    pub fn get_errors(&mut self) -> Vec<SourceContext<Error>> {
        let mut v = Vec::new();
        std::mem::swap(&mut self.errors, &mut v);
        v
    }
}

pub trait LoadScript {
    fn load(&mut self, s: &str) -> Result<Eval<Source<Value>>, Error>;
}

impl LoadScript for grease::Context<Runtime> {
    fn load(&mut self, s: &str) -> Result<Eval<Source<Value>>, Error> {
        let loadpath = match self.env_get(LOAD_PATH_BINDING) {
            Some(v) => match v {
                Eval::Error => return Ok(Eval::Error),
                Eval::Value(v) => {
                    script_value_as!(
                        self,
                        v.clone(),
                        ScriptArray,
                        format!("{} must be an array", LOAD_PATH_BINDING)
                    )
                    .owned()
                    .0
                }
            },
            None => vec![],
        };

        let mut paths: Vec<PathBuf> = Vec::new();
        paths.push(self.working_dir.clone());
        for path in loadpath {
            paths.push(
                script_value_as!(
                    self,
                    path,
                    PathBuf,
                    format!("{} item must be a path", LOAD_PATH_BINDING)
                )
                .to_path_buf(),
            );
        }

        let full_path = paths.iter().find_map(|path| {
            let mut p = path.to_path_buf();
            p.push(s);
            if p.is_file() {
                Some(p)
            } else {
                None
            }
        });
        if let Some(p) = full_path {
            let p = p.canonicalize().unwrap(); // unwrap because is_file() should guarantee that canonicalize will succeed
            if !self.load_cache.contains_key(&p) {
                let mut old_work_dir = p.parent().unwrap().into(); // unwrap because is_file() necessitates a parent exists
                std::mem::swap(&mut old_work_dir, &mut self.working_dir);
                let result =
                    super::Script::load(Source::new(super::ast::FileSource(p.clone())))?.plan(self);
                self.load_cache.insert(p.clone(), result);
                std::mem::swap(&mut old_work_dir, &mut self.working_dir);
            }
            Ok(self.load_cache.get(&p).unwrap().clone()) // unwrap because prior statement ensures the key exists
        } else {
            Err(Error::LoadFailed(s.to_owned()))
        }
    }
}

/// Function call arguments interface.
#[derive(Debug)]
pub struct UncheckedFunctionArguments {
    pub positional: std::iter::Peekable<<Vec<Source<Value>> as IntoIterator>::IntoIter>,
    pub non_positional: BTreeMap<String, Source<Value>>,
}

impl UncheckedFunctionArguments {
    fn new(
        positional: Vec<Source<Value>>,
        non_positional: BTreeMap<String, Source<Value>>,
    ) -> Self {
        UncheckedFunctionArguments {
            positional: positional.into_iter().peekable(),
            non_positional,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn len(&self) -> usize {
        self.positional.len()
    }

    pub fn kw(&mut self, key: &str) -> Option<Source<Value>> {
        self.non_positional.remove(key)
    }

    pub fn peek(&mut self) -> Option<&Source<Value>> {
        self.positional.peek()
    }

    pub fn clear(&mut self) {
        while self.positional.next().is_some() {}
        self.non_positional.clear()
    }
}

impl Default for UncheckedFunctionArguments {
    fn default() -> Self {
        Self::new(Default::default(), Default::default())
    }
}

impl Iterator for UncheckedFunctionArguments {
    type Item = Source<Value>;

    fn next(&mut self) -> Option<Self::Item> {
        self.positional.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.positional.size_hint()
    }
}

/// Function call arguments.
///
/// Checks whether all arguments have been consumed.
#[derive(Debug, Default)]
pub struct FunctionArguments {
    inner: UncheckedFunctionArguments,
}

impl FunctionArguments {
    pub fn new(
        positional: Vec<Source<Value>>,
        non_positional: BTreeMap<String, Source<Value>>,
    ) -> Self {
        FunctionArguments {
            inner: UncheckedFunctionArguments::new(positional, non_positional),
        }
    }

    pub fn positional(positional: Vec<Source<Value>>) -> Self {
        Self::new(positional, Default::default())
    }

    pub fn unchecked(mut self) -> UncheckedFunctionArguments {
        std::mem::take(&mut self.inner)
    }
}

impl Iterator for FunctionArguments {
    type Item = Source<Value>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl std::ops::Deref for FunctionArguments {
    type Target = UncheckedFunctionArguments;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl std::ops::DerefMut for FunctionArguments {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl Drop for FunctionArguments {
    /// Asserts that the function arguments have all been consumed prior to being dropped.
    fn drop(&mut self) {
        assert!(self.inner.is_empty() && self.inner.non_positional.is_empty());
    }
}

impl From<UncheckedFunctionArguments> for FunctionArguments {
    fn from(inner: UncheckedFunctionArguments) -> Self {
        FunctionArguments { inner }
    }
}

/// Function call context.
pub struct FunctionContext {
    ctx: Runtime,
    args: FunctionArguments,
}

impl FunctionContext {
    #[must_use]
    pub fn unused_non_positional(&mut self) -> bool {
        let kw = std::mem::take(&mut self.args.non_positional);
        let ret = !kw.is_empty();
        for e in kw
            .into_iter()
            .map(|(k, v)| v.with(Error::UnexpectedNonPositionalArgument(k)))
        {
            self.ctx.error(e)
        }
        ret
    }

    #[must_use]
    pub fn unused_positional(&mut self) -> bool {
        let ret = !self.args.is_empty();
        while let Some(v) = self.args.next() {
            self.ctx.error(v.with(Error::UnexpectedPositionalArguments));
        }
        ret
    }

    #[must_use]
    pub fn unused_arguments(&mut self) -> bool {
        self.unused_positional() | self.unused_non_positional()
    }
}

impl std::ops::Deref for FunctionContext {
    type Target = Runtime;

    fn deref(&self) -> &Self::Target {
        &self.ctx
    }
}

impl std::ops::DerefMut for FunctionContext {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.ctx
    }
}

impl grease::SplitInto<Runtime> for FunctionContext {
    type Extra = FunctionArguments;

    fn split(self) -> (Runtime, Self::Extra) {
        (self.ctx, self.args)
    }

    fn join(ctx: Runtime, args: Self::Extra) -> Self {
        FunctionContext { ctx, args }
    }
}

/// Script runtime errors.
#[derive(Debug)]
pub enum Error {
    /// An integer index (for arrays) was expected.
    NonIntegerIndex,
    /// An index was not present.
    MissingIndex(String),
    /// An indexing operation was attempted on a type that is not an array or map.
    InvalidIndex,
    /// An expression is in call-position (had arguments) but is not callable.
    NonCallableExpression(Value),
    /// No binding with the given name is available in the current environment.
    MissingBinding(String),
    /// A merge expression cannot be evaluated.
    CannotMerge(String),
    /// A map pattern has too many rest patterns.
    PatternMapTooManyRest,
    /// A map pattern had unmatched keys.
    PatternMapExtraKeys(Value),
    /// A rest pattern in an array is undecidable.
    PatternArrayRestUndecidable,
    /// A pattern did not match a value.
    PatternMismatch(Value),
    /// A command pattern has too many non-positional patterns.
    PatternCommandTooManyNonPositional,
    /// No patterns in a match expression matched a value.
    MatchFailed(Value),
    /// A command does not accept non-positional arguments.
    NoNonPositionalArguments,
    /// A command does not accept a particular non-positional argument.
    UnexpectedNonPositionalArgument(String),
    /// A command does not accept one or more positional arguments.
    UnexpectedPositionalArguments,
    /// Arguments to a function did not match the function definition.
    ArgumentMismatch,
    /// A load failed to find a module.
    LoadFailed(String),
    /// An error occured while evaluating a value.
    ValueError(grease::Error),
    /// An error occured while loading a script.
    ScriptLoadError(super::ast::Error),
    /// A generic error message.
    GenericError(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Error::*;
        match self {
            NonIntegerIndex => write!(f, "positive integer index expected"),
            MissingIndex(s) => write!(f, "index missing: {}", s),
            InvalidIndex => write!(f, "type is not an array or map; cannot index"),
            NonCallableExpression(_) => write!(f, "cannot pass arguments to non-callable value"),
            MissingBinding(s) => write!(f, "'{}' is not available in the current environment", s),
            CannotMerge(s) => write!(f, "cannot merge: {}", s),
            PatternMapTooManyRest => write!(f, "map pattern may only have one merge subpattern"),
            PatternMapExtraKeys(_) => write!(f, "map pattern doesn't match all values in map"),
            PatternArrayRestUndecidable => write!(f, "array merge pattern is undecidable"),
            PatternMismatch(_) => write!(f, "value could not be matched to pattern"),
            PatternCommandTooManyNonPositional => write!(
                f,
                "command patterns can only have one non-positional pattern"
            ),
            MatchFailed(_) => write!(f, "no patterns matched the value"),
            NoNonPositionalArguments => {
                write!(f, "the function does not accept non-positional arguments")
            }
            UnexpectedNonPositionalArgument(s) => write!(
                f,
                "the function does not accept a non-positional argument with key '{}'",
                s
            ),
            UnexpectedPositionalArguments => write!(f, "extraneous positional arguments"),
            ArgumentMismatch => write!(f, "argument mismatch in command"),
            LoadFailed(s) => write!(f, "failed to find module matching {}", s),
            ValueError(e) => write!(f, "{}", e),
            ScriptLoadError(e) => write!(f, "{}", e),
            GenericError(s) => write!(f, "{}", s),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::ValueError(e) => Some(e.as_ref()),
            Error::ScriptLoadError(e) => Some(e),
            _ => None,
        }
    }
}

impl From<Error> for std::io::Error {
    fn from(e: Error) -> Self {
        std::io::Error::new(std::io::ErrorKind::Other, e)
    }
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        e.to_string().into()
    }
}

impl From<grease::Error> for Error {
    fn from(e: grease::Error) -> Self {
        Error::ValueError(e)
    }
}

impl From<super::ast::Error> for Error {
    fn from(e: super::ast::Error) -> Self {
        Error::ScriptLoadError(e)
    }
}

impl From<&'_ str> for Error {
    fn from(s: &str) -> Self {
        Self::from(s.to_owned())
    }
}

impl From<String> for Error {
    fn from(s: String) -> Self {
        Error::GenericError(s)
    }
}

impl From<Source<&'_ str>> for Source<Error> {
    fn from(s: Source<&str>) -> Self {
        Self::from(s.map(ToOwned::to_owned))
    }
}

impl From<Source<String>> for Source<Error> {
    fn from(s: Source<String>) -> Self {
        s.map(Error::GenericError)
    }
}

#[derive(Debug)]
pub struct SourceContext<T> {
    value: Source<T>,
    context: Vec<Source<String>>,
}

impl<T> SourceContext<T> {
    pub fn add_context(&mut self, ctx: Source<String>) {
        self.context.push(ctx)
    }
}

impl<T: ToString> SourceContext<T> {
    pub fn nest_into<U, V>(self, v: V) -> SourceContext<U>
    where
        V: Into<SourceContext<U>>,
    {
        let mut ret = v.into();
        ret.context.push(self.value.map(|v| v.to_string()));
        ret.context.extend(self.context);
        ret
    }

    pub fn nest<V: Into<Self>>(self, v: V) -> Self {
        self.nest_into(v)
    }
}

impl<U> From<Source<U>> for SourceContext<Error>
where
    Error: From<U>,
{
    fn from(u: Source<U>) -> SourceContext<Error> {
        SourceContext {
            value: u.map(Error::from),
            context: vec![],
        }
    }
}

impl<T> std::iter::Extend<Source<String>> for SourceContext<T> {
    fn extend<U>(&mut self, iter: U)
    where
        U: IntoIterator<Item = Source<String>>,
    {
        self.context.extend(iter);
    }
}

impl<T, U: ToString> std::iter::Extend<SourceContext<U>> for SourceContext<T> {
    fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = SourceContext<U>>,
    {
        for i in iter {
            self.context.push(i.value.map(|v| v.to_string()));
            self.context.extend(i.context);
        }
    }
}

impl<T: fmt::Display> fmt::Display for SourceContext<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}", self.value)?;
        for c in &self.context {
            writeln!(f, "{}", c)?;
        }
        Ok(())
    }
}

macro_rules! value_now {
    ( $e:expr ) => {
        $e.get().map_err(move |e| Error::ValueError(e))?
    };
}

#[derive(Clone, Debug, PartialEq)]
pub enum Eval<T> {
    Value(T),
    Error,
}

impl<T> Eval<T> {
    pub fn map<F, U>(self, f: F) -> Eval<U>
    where
        F: FnOnce(T) -> U,
    {
        match self {
            Eval::Value(v) => Eval::Value(f(v)),
            Eval::Error => Eval::Error,
        }
    }

    pub fn then<F, U>(self, f: F) -> Eval<U>
    where
        F: FnOnce(T) -> Eval<U>,
    {
        match self {
            Eval::Error => Eval::Error,
            Eval::Value(v) => f(v),
        }
    }

    pub fn as_ref(&self) -> Eval<&T> {
        match *self {
            Eval::Error => Eval::Error,
            Eval::Value(ref v) => Eval::Value(v),
        }
    }

    pub fn as_mut(&mut self) -> Eval<&mut T> {
        match *self {
            Eval::Error => Eval::Error,
            Eval::Value(ref mut v) => Eval::Value(v),
        }
    }
}

impl<T> From<T> for Eval<T> {
    fn from(v: T) -> Self {
        Eval::Value(v)
    }
}

impl<T, V> FromIterator<Eval<T>> for Eval<V>
where
    V: FromIterator<T>,
{
    fn from_iter<I: IntoIterator<Item = Eval<T>>>(iter: I) -> Self {
        let mut bad = false;
        let r = V::from_iter(iter.into_iter().filter_map(|v| {
            if bad {
                return None;
            }
            match v {
                Eval::Error => {
                    bad = true;
                    None
                }
                Eval::Value(v) => Some(v),
            }
        }));
        if bad {
            Eval::Error
        } else {
            Eval::Value(r)
        }
    }
}

impl<T> std::ops::BitAndAssign for Eval<T> {
    fn bitand_assign(&mut self, other: Eval<T>) {
        if let Eval::Value(a) = self {
            if let Eval::Value(b) = other {
                *a = b;
                return;
            }
        }
        *self = Eval::Error;
    }
}

/// Apply the value to the given arguments.
///
/// If `env_lookup` is true and `v` is a `ScriptString`, it will be looked up in the environment.
pub fn apply_value(
    ctx: &mut grease::Context<Runtime>,
    v: Source<Value>,
    mut args: UncheckedFunctionArguments,
    env_lookup: bool,
) -> Result<Eval<Value>, Error> {
    let v_source = v.source();

    let result = v.map(|v| {
        let v = if env_lookup {
            match v.typed::<ScriptString>() {
                Ok(val) => {
                    let s = value_now!(val).owned();
                    trace!("looking up '{}' in environment", s);
                    // Lookup string in environment, and apply result to remaining arguments
                    match ctx.inner.env_get(&s) {
                        Some(Eval::Value(value)) => {
                            trace!("found match in environment for '{}': {}", s, value.id());
                            value.clone().unwrap()
                        }
                        Some(Eval::Error) => return Ok(Eval::Error),
                        None => {
                            return Err(Error::MissingBinding(s));
                        }
                    }
                }
                Err(v) => v,
            }
        } else {
            v
        };

        if args.peek().is_none() {
            return Ok(v.into());
        }

        match_value!(v => {
            ScriptArray => |val| {
                let index = eval_error!(ctx, args.next().unwrap().map(|v|
                    v.typed::<ScriptString>()
                        .map_err(|_| "index must be a string".into())
                        .and_then(|v| v.get())
                    )
                    .transpose());
                let val = eval_error!(ctx, index.map(|index| match usize::from_str(index.as_ref()) {
                    Err(_) => Err(Error::NonIntegerIndex),
                    Ok(ind) => value_now!(val).0.get(ind).cloned().map(Source::unwrap)
                        .ok_or(Error::MissingIndex(index.to_owned()))
                }).transpose());

                let (source,val) = val.take();

                apply_value(ctx, Source::from((v_source,source)).with(val), args, false)
            },
            ScriptMap => |val| {
                let index = eval_error!(ctx, args.next().unwrap().map(|v|
                    v.typed::<ScriptString>()
                        .map_err(|_| "index must be a string".into())
                        .and_then(|v| v.get())
                    )
                    .transpose());

                let val = eval_error!(ctx, index.map(|index|
                        value_now!(val).0.get(index.as_ref()).cloned().map(Source::unwrap)
                            .ok_or(Error::MissingIndex(index.as_ref().into()))
                    )
                    .transpose());

                let (source,val) = val.take();

                apply_value(ctx, Source::from((v_source,source)).with(val), args, false)
            },
            ScriptFunction => |val| {
                value_now!(val).plan_join(ctx, args.into())
            },
            => |v| {
                Err(Error::NonCallableExpression(v).into())
            }
        })
    })
    .transpose_err();
    Ok(eval_error!(ctx, result))
}

#[derive(Clone, Debug)]
struct PatternValues {
    pub literal: Value,
    pub binding: Value,
}

impl PatternValues {
    pub fn new(literal: Value, binding: Value) -> Self {
        PatternValues { literal, binding }
    }

    pub fn singular(v: Value) -> Self {
        Self::new(v.clone(), v)
    }
}

pub fn apply_pattern(
    ctx: &mut grease::Context<Runtime>,
    pat: Pat,
    val: Eval<Source<Value>>,
) -> Result<Env, Vec<SourceContext<Error>>> {
    let mut ret = Env::new();
    let mut errs = Vec::new();

    _apply_pattern(
        ctx,
        &mut ret,
        &mut errs,
        pat,
        val.map(|v| v.map(PatternValues::singular)),
    );
    if errs.is_empty() {
        Ok(ret)
    } else {
        Err(errs)
    }
}

fn _apply_pattern(
    ctx: &mut grease::Context<Runtime>,
    env: &mut Env,
    errs: &mut Vec<SourceContext<Error>>,
    pat: Pat,
    val: Eval<Source<PatternValues>>,
) {
    use Pattern::*;
    let (source, pat) = pat.take();
    let desc = std::mem::discriminant(&pat);
    match pat {
        Any => (),
        Literal(e) => match (e.plan(ctx), val) {
            (Eval::Value(result), Eval::Value(val)) => {
                if *result != (*val).literal {
                    let mut err: SourceContext<Error> = source
                        .with(Error::PatternMismatch((*result).clone()))
                        .into();
                    err.add_context(result.with("value definition".into()));
                    errs.push(err);
                }
            }
            _ => (),
        },
        Binding(name) => {
            env.insert(name, val.map(|v| v.map(|v| v.binding)));
            ()
        }
        Array(inner) => {
            let mut orig_val = None;
            let val = val.then(|v| {
                orig_val = Some(v.clone());
                let (vsource, v) = v.take();
                match v.binding.typed::<ScriptArray>() {
                    Ok(v) => match v.get() {
                        Ok(v) => Eval::Value(v.owned().0),
                        Err(e) => {
                            errs.push(vsource.with(Error::ValueError(e)).into());
                            Eval::Error
                        }
                    },
                    Err(o) => {
                        let mut err: SourceContext<Error> =
                            source.clone().with(Error::PatternMismatch(o)).into();
                        err.add_context(vsource.with("value definition".into()));
                        errs.push(err);
                        Eval::Error
                    }
                }
            });
            let vals: Vec<Eval<Source<PatternValues>>> = match val {
                Eval::Error => std::iter::repeat(Eval::Error).take(inner.len()).collect(),
                Eval::Value(v) => v
                    .into_iter()
                    .enumerate()
                    .map(|(i, v)| {
                        Eval::Value(v.map(|v| {
                            PatternValues::new(
                                v.clone(),
                                v.set_dependencies(depends![
                                    orig_val.as_ref().unwrap().as_ref().unwrap().binding,
                                    desc,
                                    i
                                ]),
                            )
                        }))
                    })
                    .collect(),
            };

            match pattern_array(ctx, &inner, &vals) {
                Ok(mut n_env) => {
                    env.append(&mut n_env);
                }
                Err(n_errs) => {
                    for e in n_errs {
                        errs.push(match &orig_val {
                            None => e.nest(
                                source
                                    .clone()
                                    .with(Error::ValueError("pattern could not be matched".into())),
                            ),
                            Some(orig_val) => {
                                let mut err = e.nest(
                                    source
                                        .clone()
                                        .with(Error::PatternMismatch(orig_val.binding.clone())),
                                );
                                err.add_context(
                                    orig_val.source().with("value being matched".into()),
                                );
                                err
                            }
                        });
                    }
                }
            }
        }
        Map(inner) => {
            let mut val = val.then(|v| {
                let orig = v.clone();
                let (vsource, v) = v.take();
                match v.binding.typed::<ScriptMap>() {
                    Ok(v) => match v.get() {
                        Ok(v) => Eval::Value((orig, v.owned().0)),
                        Err(e) => {
                            errs.push(vsource.with(Error::ValueError(e)).into());
                            Eval::Error
                        }
                    },
                    Err(o) => {
                        let mut err: SourceContext<Error> =
                            source.clone().with(Error::PatternMismatch(o)).into();
                        err.add_context(vsource.with("value definition".into()));
                        errs.push(err);
                        Eval::Error
                    }
                }
            });
            let mut rest_pattern = None;
            let mut matched_keys = BTreeMap::new();
            for i in inner {
                let (isource, i) = i.take();
                let itemdesc = std::mem::discriminant(&i);
                match i {
                    MapPattern::Item(key, pat) => {
                        let result = match val.as_mut() {
                            Eval::Value((orig_value, m)) => match m.remove(&key) {
                                Some(v) => {
                                    let keydep = depends![key];
                                    matched_keys.insert(key, v.source());
                                    Eval::Value(v.clone().map(|v| {
                                        PatternValues::new(
                                            v.clone(),
                                            v.set_dependencies(depends![join
                                                depends![orig_value.as_ref().binding,desc,itemdesc],
                                                keydep
                                            ]),
                                        )
                                    }))
                                }
                                None => {
                                    let (vsource, v) = orig_value.clone().take();
                                    let mut err: SourceContext<Error> = source
                                        .clone()
                                        .with(Error::PatternMismatch(v.binding))
                                        .into();
                                    err.add_context(isource.with(format!("missing key '{}'", key)));
                                    if let Some(src) = matched_keys.get(&key) {
                                        err.add_context(
                                            src.clone().with("key previously matched here".into()),
                                        );
                                    }
                                    err.add_context(vsource.with("value definition".into()));
                                    errs.push(err);
                                    Eval::Error
                                }
                            },
                            Eval::Error => Eval::Error,
                        };
                        _apply_pattern(ctx, env, errs, pat, result);
                    }
                    MapPattern::Rest(pat) => {
                        if let Some((src, _)) = rest_pattern.replace((isource.clone(), pat)) {
                            let mut err: SourceContext<Error> =
                                isource.with(Error::PatternMapTooManyRest).into();
                            err.add_context(src.with("previous definition".into()));
                            errs.push(err);
                        }
                    }
                }
            }

            // Match rest pattern with remaining values
            if let Some((src, rest)) = rest_pattern {
                _apply_pattern(
                    ctx,
                    env,
                    errs,
                    rest,
                    val.map(move |(_, m)| src.with(PatternValues::singular(ScriptMap(m).into()))),
                )
            } else {
                match val {
                    Eval::Value((orig_value, m)) => {
                        if !m.is_empty() {
                            let (vsource, v) = orig_value.take();
                            let mut err: SourceContext<Error> =
                                source.with(Error::PatternMapExtraKeys(v.binding)).into();
                            err.add_context(vsource.with("value definition".into()));
                            errs.push(err);
                        }
                    }
                    _ => (),
                }
            }
        }
    }
}

fn is_fixed_point(pat: &Pattern) -> bool {
    use Pattern::*;
    match pat {
        Any | Binding(_) => false,
        _ => true,
    }
}

fn pattern_array(
    ctx: &mut grease::Context<Runtime>,
    pats: &[Source<ArrayPattern>],
    vals: &[Eval<Source<PatternValues>>],
) -> Result<Env, Vec<SourceContext<Error>>> {
    let mut ret = Env::new();
    let mut errs = Vec::new();

    _pattern_array(ctx, &mut ret, &mut errs, pats, vals);
    if errs.is_empty() {
        Ok(ret)
    } else {
        Err(errs)
    }
}

fn _pattern_array(
    ctx: &mut grease::Context<Runtime>,
    env: &mut Env,
    errs: &mut Vec<SourceContext<Error>>,
    pats: &[Source<ArrayPattern>],
    vals: &[Eval<Source<PatternValues>>],
) {
    let mut vali = 0;
    let mut pati = 0;
    loop {
        let pat = if let Some(i) = pats.get(pati) {
            pati += 1;
            i
        } else {
            if vali != vals.len() {
                let err: SourceContext<Error> = pats
                    .into_source()
                    .with(Error::ValueError(
                        "not enough patterns to match with value items".into(),
                    ))
                    .into();
                errs.push(err);
            }
            break;
        };
        let (psource, pat) = pat.as_ref().take();
        match pat {
            ArrayPattern::Item(p) => {
                if let Some(v) = vals.get(vali) {
                    vali += 1;
                    _apply_pattern(ctx, env, errs, p.clone(), v.clone());
                } else {
                    let err: SourceContext<Error> = psource
                        .with(Error::ValueError("no nested value matches pattern".into()))
                        .into();
                    errs.push(err);
                    break;
                }
            }
            ArrayPattern::Rest(p) => {
                let rest_end = pattern_array_rest(ctx, env, errs, &pats[pati..], &vals[vali..]);
                _apply_pattern(
                    ctx,
                    env,
                    errs,
                    p.clone(),
                    rest_end.map(|vs| {
                        vs.into_source().map(|vs| {
                            let mut bindings = Vec::new();
                            let mut literals = Vec::new();
                            for (source, p) in vs.into_iter().map(|v| v.take()) {
                                bindings.push(source.clone().with(p.binding));
                                literals.push(source.with(p.literal));
                            }
                            PatternValues::new(
                                ScriptArray(literals).into_value(),
                                ScriptArray(bindings).into_value(),
                            )
                        })
                    }),
                );
                break;
            }
        }
    }
}

fn pattern_array_rest(
    ctx: &mut grease::Context<Runtime>,
    env: &mut Env,
    errs: &mut Vec<SourceContext<Error>>,
    pats: &[Source<ArrayPattern>],
    vals: &[Eval<Source<PatternValues>>],
) -> Eval<Vec<Source<PatternValues>>> {
    let pat = if let Some(i) = pats.first() {
        i
    } else {
        return vals.into_iter().cloned().collect::<Eval<Vec<_>>>();
    };
    let (psource, pat) = pat.as_ref().take();
    match pat {
        ArrayPattern::Item(p) => {
            if is_fixed_point(&*p) {
                // If we have a fixed point pattern, try to match at different locations
                let mut vali = 0;
                while vali < vals.len() {
                    match pattern_array(ctx, &pats, &vals[vali..]) {
                        Ok(mut n_env) => {
                            env.append(&mut n_env);
                            return vals[..vali].into_iter().cloned().collect::<Eval<Vec<_>>>();
                        }
                        Err(_) => (),
                    }
                    vali += 1;
                }
                errs.push(
                    psource
                        .with(Error::ValueError(
                            "no value matched the given pattern".into(),
                        ))
                        .into(),
                );
                Eval::Error
            } else {
                // Go forward to try to find a fixed point
                pattern_array_rest(ctx, env, errs, &pats[1..], &vals[..]).then(|values| {
                    if values.is_empty() {
                        errs.push(
                            psource
                                .with(Error::ValueError("no value to match with pattern".into()))
                                .into(),
                        );
                        Eval::Error
                    } else {
                        let mut remaining = values.len() - 1;
                        let mut ret = Vec::with_capacity(remaining);
                        let mut val = None;
                        for v in values {
                            if remaining > 0 {
                                ret.push(v);
                                remaining -= 1;
                            } else {
                                val = Some(v);
                            }
                        }
                        _apply_pattern(ctx, env, errs, p.clone(), Eval::Value(val.unwrap()));
                        Eval::Value(ret)
                    }
                })
            }
        }
        ArrayPattern::Rest(_) => {
            errs.push(psource.with(Error::PatternArrayRestUndecidable).into());
            return Eval::Error;
        }
    }
}

pub fn apply_command_pattern(
    ctx: &mut grease::Context<Runtime>,
    pat: CmdPat,
    mut args: FunctionArguments,
) -> Result<Env, Vec<SourceContext<Error>>> {
    let pat = pat.unwrap();
    let kw = std::mem::take(&mut args.non_positional);
    let vals: Vec<_> = args
        .map(|v| Eval::Value(v.map(PatternValues::singular)))
        .collect();

    // Partition non-positional/positional argument patterns
    let mut pos_args = Vec::new();
    let mut non_pos_args = Vec::new();
    for p in pat {
        let (psource, p) = p.take();
        if let ArrayPattern::Rest(p) = p {
            if let Pattern::Map(_) = &*p {
                non_pos_args.push(p);
            } else {
                pos_args.push(psource.with(ArrayPattern::Rest(p)));
            }
        } else {
            pos_args.push(psource.with(p));
        }
    }

    // If more than one non-positional pattern, error
    if non_pos_args.len() > 1 {
        let vals = non_pos_args.get(1..).unwrap();
        let mut err: SourceContext<Error> = non_pos_args
            .as_slice()
            .into_source()
            .with(Error::PatternCommandTooManyNonPositional)
            .into();
        for a in vals {
            err.add_context(a.source().with("extra non-positional argument".into()));
        }
        Err(vec![err])
    } else {
        // Get non-positional pattern bindings
        let non_pos_bindings = if let Some(non_pos_arg) = non_pos_args.into_iter().next() {
            apply_pattern(
                ctx,
                non_pos_arg,
                Eval::Value(kw.into_source().map(|kw| ScriptMap(kw).into_value())),
            )
        } else if !kw.is_empty() {
            Err(kw
                .into_iter()
                .map(|(k, v)| v.with(Error::UnexpectedNonPositionalArgument(k)).into())
                .collect())
        } else {
            Ok(Env::default())
        };
        // Merge with positional pattern bindings, if applicable
        let pos_bindings = pattern_array(ctx, &pos_args, &vals);
        match (non_pos_bindings, pos_bindings) {
            (Ok(mut a), Ok(mut b)) => {
                a.append(&mut b);
                Ok(a)
            }
            (Err(mut a), Err(mut b)) => {
                a.append(&mut b);
                Err(a)
            }
            (a, b) => a.and(b),
        }
    }
}

impl Plan<FunctionContext> for &'_ ScriptFunction {
    type Output = Result<Eval<Value>, Error>;

    fn plan(self, ctx: &mut grease::Context<FunctionContext>) -> Self::Output {
        match self {
            ScriptFunction::UserFunction(pat, e, env) => {
                let src = e.source();

                let mut env = vec![env.clone()];
                std::mem::swap(&mut env, &mut ctx.inner.env);
                let args = std::mem::take(&mut ctx.args);
                use grease::SplitInto;
                let ret =
                    match ctx.split_map(move |ctx| apply_command_pattern(ctx, pat.clone(), args)) {
                        Ok(bindings) => {
                            ctx.inner.env.push(bindings);
                            e.clone().plan_split(ctx).map(Source::unwrap)
                        }
                        Err(errs) => {
                            let mut err = SourceContext::from(src.with(Error::ArgumentMismatch));
                            err.extend(errs);
                            ctx.error(err);
                            Eval::Error
                        }
                    };

                std::mem::swap(&mut env, &mut ctx.inner.env);

                Ok(ret)
            }
            ScriptFunction::BuiltinFunction(f) => match f(ctx) {
                Err(e) => {
                    ctx.args.clear();
                    Err(e)
                }
                v => v,
            },
        }
    }
}

impl Plan<Runtime> for Expression {
    type Output = Result<Eval<Value>, Error>;

    fn plan(self, ctx: &mut grease::Context<Runtime>) -> Self::Output {
        use Expression::*;
        match self {
            Empty => Ok(().into_value().into()),
            Expression::String(s) => Ok(s.into_value().into()),
            Array(es) => {
                let all_vals = es
                    .into_iter()
                    .map(|e| e.plan(ctx))
                    .collect::<Eval<Vec<_>>>();
                let all_vals = match all_vals {
                    Eval::Error => return Ok(Eval::Error),
                    Eval::Value(v) => v,
                };
                let mut had_error = false;
                let mut vals = Vec::new();
                for v in all_vals {
                    let (_merge_source, (merge, val)) = v.take();
                    if merge {
                        let (val_source, val) = val.take();
                        match val.typed::<ScriptArray>() {
                            Ok(val) => vals.extend(value_now!(val).owned().0),
                            Err(_) => {
                                ctx.error(
                                    val_source.with(Error::CannotMerge("non-array value".into())),
                                );
                                had_error = true;
                            }
                        }
                    } else {
                        vals.push(val);
                    }
                }

                Ok(if had_error {
                    Eval::Error
                } else {
                    Eval::Value(ScriptArray(vals).into_value())
                })
            }
            Set(pat, e) => {
                let data = e.plan(ctx);
                let ok_ret = match data {
                    Eval::Error => Eval::Error,
                    _ => ().into_value().into(),
                };
                Ok(match apply_pattern(ctx, *pat, data) {
                    Ok(env) => {
                        ctx.env_extend(env);
                        ok_ret
                    }
                    Err(errs) => {
                        for e in errs {
                            ctx.error(e);
                        }
                        Eval::Error
                    }
                })
            }
            Unset(var) => {
                ctx.inner.env_remove(&var);
                Ok(().into_value().into())
            }
            Command(cmd, args) => {
                let f = cmd.plan(ctx);
                let args = args
                    .into_iter()
                    .map(|a| a.plan(ctx))
                    .collect::<Eval<Vec<_>>>();
                let args = match args {
                    Eval::Error => return Ok(Eval::Error),
                    Eval::Value(v) => v,
                };

                let mut had_error = false;
                let mut vals = Vec::new();
                let mut kw_vals = BTreeMap::new();
                for a in args {
                    let (_merge_source, (merge, val)) = a.take();
                    if merge {
                        let (val_source, val) = val.take();
                        match_value!(val => {
                            ScriptArray => |val| {
                                vals.extend(value_now!(val).owned().0);
                            },
                            ScriptMap => |val| {
                                kw_vals.extend(value_now!(val).owned().0);
                            },
                            => |_| {
                                ctx.error(
                                    val_source.with(Error::CannotMerge("non-array/map value".into())),
                                );
                                had_error = true;
                            }
                        });
                    } else {
                        vals.push(val);
                    }
                }

                if had_error {
                    return Ok(Eval::Error);
                }

                match f {
                    Eval::Value(f) => {
                        apply_value(ctx, f, UncheckedFunctionArguments::new(vals, kw_vals), true)
                    }
                    Eval::Error => Ok(Eval::Error),
                }
            }
            Block(_) => panic!("Block expression must be evaluated at a higher level"),
            Function(pat, e) => Ok(Eval::Value(
                ScriptFunction::UserFunction(pat, *e, ctx.inner.env_flatten()).into_value(),
            )),
            If(cond, t, f) => {
                let cond = match cond.plan(ctx) {
                    Eval::Value(v) => v.unwrap(),
                    Eval::Error => return Ok(Eval::Error),
                };

                let to_bool: Option<grease::IntoTyped<bool>> = ctx.traits.get(&cond);
                let cond = match to_bool {
                    Some(t) => {
                        let v = value_now!(t.into_typed(cond));
                        *v
                    }
                    None => true,
                };
                Ok(if cond { t.plan(ctx) } else { f.plan(ctx) }.map(Source::unwrap))
            }
            Match(val, pats) => {
                let val = match val.plan(ctx) {
                    Eval::Value(v) => v,
                    Eval::Error => return Ok(Eval::Error),
                };

                for (p, e) in pats {
                    if let Ok(env) = apply_pattern(ctx, p, Eval::Value(val.clone())) {
                        ctx.inner.env.push(env);
                        let ret = e.plan(ctx);
                        ctx.inner.env.pop();
                        return Ok(ret.map(Source::unwrap));
                    }
                }

                Err(Error::MatchFailed(val.unwrap()))
            }
        }
    }
}

impl Plan<Runtime> for Source<Expression> {
    type Output = Eval<Source<Value>>;

    fn plan(self, ctx: &mut grease::Context<Runtime>) -> Self::Output {
        let (source, expr) = self.take();
        if let Expression::Block(es) = expr {
            source.with(es).plan(ctx)
        } else {
            match expr.plan(ctx) {
                Ok(val) => val.map(|v| source.with(v)),
                Err(e) => {
                    ctx.error(source.with(e));
                    Eval::Error
                }
            }
        }
    }
}

impl Plan<Runtime> for Source<MergeExpression> {
    type Output = Eval<Source<(bool, Source<Value>)>>;

    fn plan(self, ctx: &mut grease::Context<Runtime>) -> Self::Output {
        let (source, val) = self.take();
        let merge = val.merge;
        val.expr.plan(ctx).map(move |e| source.with((merge, e)))
    }
}

impl Plan<Runtime> for Source<Vec<Source<MergeExpression>>> {
    type Output = Eval<Source<Value>>;

    fn plan(self, ctx: &mut grease::Context<Runtime>) -> Self::Output {
        // Push a new scope
        ctx.inner.env.push(Default::default());
        let (self_source, this) = self.take();
        let mut val = Eval::Value(self_source.clone().with(().into_value()));
        for e in this {
            val &= match e.plan(ctx) {
                Eval::Value(merge) => {
                    let (merge_source, (merge, val)) = merge.take();
                    if merge {
                        let (val_source, val) = val.take();
                        match val.typed::<ScriptMap>() {
                            Ok(val) => match val.get() {
                                Ok(val) => {
                                    for (k, v) in val.owned().0 {
                                        ctx.env_insert(k, Eval::Value(v));
                                    }
                                    Eval::Value(merge_source.with(().into_value()))
                                }
                                Err(e) => {
                                    ctx.error(val_source.with(e));
                                    Eval::Error
                                }
                            },
                            Err(_) => {
                                ctx.error(
                                    val_source.with(Error::CannotMerge("non-map value".into())),
                                );
                                Eval::Error
                            }
                        }
                    } else {
                        Eval::Value(val)
                    }
                }
                Eval::Error => Eval::Error,
            };
        }

        // Pop the pushed scope
        let ret = ctx.inner.env.pop().unwrap();
        let ret = ret
            .into_iter()
            .map(|(k, v)| v.map(move |v| (k, v)))
            .collect::<Eval<BTreeMap<_, _>>>();

        // Return result based on final value.
        match val {
            Eval::Error => Eval::Error,
            Eval::Value(val) => {
                let (source, val) = val.take();
                match val.typed::<ScriptUnit>() {
                    Ok(_) => ret.map(|ret| self_source.with(ScriptMap(ret).into_value())),
                    Err(v) => Eval::Value(source.with(v)),
                }
            }
        }
    }
}
