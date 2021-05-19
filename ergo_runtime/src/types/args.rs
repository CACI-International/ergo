//! The Args and PatternArgs types.

use crate as ergo_runtime;
use crate::abi_stable::{bst::BstMap, std_types::RVec, StableAbi};
use crate::traits;
use crate::type_system::{ergo_traits_fn, ErgoType};
use crate::{depends, Context, Dependencies, Error, Source, TypedValue, Value};
use std::collections::BTreeMap;

/// Arguments in a function call.
#[derive(Clone, ErgoType, StableAbi)]
#[repr(C)]
pub struct Args {
    pub args: UncheckedArguments,
}

/// Arguments in a pattern function call.
#[derive(Clone, ErgoType, StableAbi)]
#[repr(C)]
pub struct PatternArgs {
    pub args: UncheckedArguments,
}

impl From<&'_ Args> for Dependencies {
    fn from(a: &'_ Args) -> Self {
        depends![Args::ergo_type(), ^&a.args]
    }
}

impl From<Args> for TypedValue<Args> {
    fn from(v: Args) -> Self {
        Self::constant(v)
    }
}

impl From<&'_ PatternArgs> for Dependencies {
    fn from(a: &'_ PatternArgs) -> Self {
        depends![PatternArgs::ergo_type(), ^&a.args]
    }
}

impl From<PatternArgs> for TypedValue<PatternArgs> {
    fn from(v: PatternArgs) -> Self {
        Self::constant(v)
    }
}

/// Command arguments interface.
#[derive(Clone, Debug, StableAbi)]
#[repr(C)]
pub struct UncheckedArguments {
    /// Positional arguments in this vec are in reverse order; use Iterator to access them.
    pub positional: RVec<Source<Value>>,
    pub keyed: BstMap<Source<Value>, Source<Value>>,
}

impl UncheckedArguments {
    fn new(positional: Vec<Source<Value>>, keyed: BTreeMap<Source<Value>, Source<Value>>) -> Self {
        UncheckedArguments {
            positional: positional.into_iter().rev().collect(),
            keyed: keyed.into_iter().collect(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn len(&self) -> usize {
        self.positional.len()
    }

    pub fn kw(&mut self, key: &str) -> Option<Source<Value>> {
        self.kw_value(&super::String::from(key).into())
    }

    pub fn kw_value(&mut self, key: &Value) -> Option<Source<Value>> {
        self.keyed.remove(key)
    }

    pub fn peek(&mut self) -> Option<&Source<Value>> {
        self.positional.last()
    }

    pub fn clear(&mut self) {
        self.positional.clear();
        self.keyed.clear()
    }

    /// Return whether there were unused keyed arguments, and add errors for each unused
    /// argument to the context errors.
    pub fn unused_keyed(&mut self) -> crate::Result<()> {
        let kw = std::mem::take(&mut self.keyed);
        if kw.is_empty() {
            Ok(())
        } else {
            Err(Error::aggregate(kw.into_iter().map(|(k, v)| {
                crate::source::IntoSource::into_source((k.source(), v.source()))
                    .with(UnexpectedNonPositionalArgument)
                    .into()
            })))
        }
    }

    /// Return whether there were unused positional arguments, and add errors for each unused
    /// argument to the context errors.
    pub fn unused_positional(&mut self) -> crate::Result<()> {
        if self.positional.is_empty() {
            Ok(())
        } else {
            let mut vec: Vec<Error> = Vec::new();
            while let Some(v) = self.next() {
                vec.push(v.with(UnexpectedPositionalArguments).into());
            }
            Err(Error::aggregate(vec))
        }
    }

    /// Return whether there were unused arguments (of any kind), and add errors for each unused
    /// argument to the context errors.
    pub fn unused_arguments(&mut self) -> crate::Result<()> {
        match (self.unused_positional(), self.unused_keyed()) {
            (Err(a), Err(b)) => Err(Error::aggregate(vec![a, b])),
            (Err(a), _) => Err(a),
            (_, Err(b)) => Err(b),
            _ => Ok(()),
        }
    }

    /// Create an Arguments, which checks for all values being consumed.
    pub fn checked(self) -> Arguments {
        self.into()
    }
}

impl Default for UncheckedArguments {
    fn default() -> Self {
        Self::new(Default::default(), Default::default())
    }
}

impl Iterator for UncheckedArguments {
    type Item = Source<Value>;

    fn next(&mut self) -> Option<Self::Item> {
        self.positional.pop()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(self.positional.len()))
    }
}

impl From<&UncheckedArguments> for Dependencies {
    fn from(args: &UncheckedArguments) -> Self {
        Self::ordered(
            args.positional
                .iter()
                .map(|v| crate::dependency::Dependency::from(&**v))
                .chain(
                    args.keyed
                        .iter()
                        .map(|(k, _)| crate::dependency::Dependency::from(&**k)),
                )
                .chain(
                    args.keyed
                        .iter()
                        .map(|(_, v)| crate::dependency::Dependency::from(&**v)),
                ),
        )
    }
}

/// The error returned when a function does not accept a specific non-positional argument.
#[derive(Debug)]
pub struct UnexpectedNonPositionalArgument;

impl std::fmt::Display for UnexpectedNonPositionalArgument {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "extraneous non-positional argument")
    }
}

impl std::error::Error for UnexpectedNonPositionalArgument {}

/// The error returned when a function does not accept one or more positional arguments.
#[derive(Debug)]
pub struct UnexpectedPositionalArguments;

impl std::fmt::Display for UnexpectedPositionalArguments {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "extraneous positional arguments")
    }
}

impl std::error::Error for UnexpectedPositionalArguments {}

/// Command arguments.
///
/// Checks whether all arguments have been consumed when dropped.
#[derive(Clone, Debug, Default, StableAbi)]
#[repr(C)]
pub struct Arguments {
    inner: UncheckedArguments,
}

impl Arguments {
    pub fn new(
        positional: Vec<Source<Value>>,
        keyed: BTreeMap<Source<Value>, Source<Value>>,
    ) -> Self {
        Arguments {
            inner: UncheckedArguments::new(positional, keyed),
        }
    }

    pub fn positional(positional: Vec<Source<Value>>) -> Self {
        Self::new(positional, Default::default())
    }

    pub fn unchecked(mut self) -> UncheckedArguments {
        std::mem::take(&mut self.inner)
    }
}

impl Iterator for Arguments {
    type Item = Source<Value>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl std::ops::Deref for Arguments {
    type Target = UncheckedArguments;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl std::ops::DerefMut for Arguments {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl Drop for Arguments {
    /// Asserts that the function arguments have all been consumed prior to being dropped.
    fn drop(&mut self) {
        assert!(self.inner.is_empty() && self.inner.keyed.is_empty());
    }
}

impl From<UncheckedArguments> for Arguments {
    fn from(inner: UncheckedArguments) -> Self {
        Arguments { inner }
    }
}

impl From<&Arguments> for Dependencies {
    fn from(args: &Arguments) -> Self {
        (&args.inner).into()
    }
}

async fn bind_args<F>(
    ctx: &Context,
    to: &UncheckedArguments,
    from: Source<UncheckedArguments>,
    create_bind_rest: F,
) -> crate::Result<super::Unit>
where
    F: Fn(UncheckedArguments) -> Value + Send + Sync,
{
    let (from_source, from) = from.take();
    // Bind keyed arguments first, then use any additional arguments in the `create_bind_rest` argument of `bind_array`
    let mut keyed_rest = traits::bind_map(
        ctx,
        to.keyed.clone(),
        from_source.clone().with(from.keyed),
        true,
    )
    .await?;
    // Positional arguments are stored in reverse order, but it's easiest to match them
    // in-order (especially wrt BindRest values).
    traits::bind_array(
        ctx,
        to.positional.iter().rev().cloned().collect(),
        from_source.with(from.positional.into_iter().rev().collect()),
        |first, rest| {
            let args = UncheckedArguments::new(
                rest,
                // The first BindRest gets the additional keyed arguments
                if first {
                    std::mem::take(&mut keyed_rest).into()
                } else {
                    Default::default()
                },
            );
            create_bind_rest(args)
        },
    )
    .await?;
    Ok(super::Unit)
}

async fn args_index(ctx: &Context, ind: Source<Value>, args: &UncheckedArguments) -> Value {
    let (src, ind) = crate::err_return_value!(ctx.eval_as::<super::String>(ind).await).take();
    let s = ind.as_ref().as_str();
    if s == "positional" {
        let mut pos = args.positional.clone();
        pos.reverse();
        super::Array(pos).into()
    } else if s == "keyed" {
        super::Map(args.keyed.clone()).into()
    } else {
        src.with("unknown index").into_error().into()
    }
}

ergo_traits_fn! {
    crate::ergo_type_name!(traits, Args);
    crate::ergo_type_name!(traits, PatternArgs);

    impl traits::Bind for Args {
        async fn bind(&self, arg: Source<Value>) -> Value {
            let (src, mut arg) = arg.take();
            CONTEXT.eval(&mut arg).await;

            crate::value::match_value!{ arg,
                Args { args } => {
                    bind_args(CONTEXT, &self.args, src.with(args), |args| Args { args }.into()).await.into()
                }
                super::Index(ind) => {
                    args_index(CONTEXT, ind, &self.args).await
                }
                v => traits::bind_error(CONTEXT, src.with(v)).into()
            }
        }
    }

    impl traits::Bind for PatternArgs {
        async fn bind(&self, arg: Source<Value>) -> Value {
            let (src, mut arg) = arg.take();
            CONTEXT.eval(&mut arg).await;

            crate::value::match_value!{ arg,
                PatternArgs { args } => {
                    bind_args(CONTEXT, &self.args, src.with(args), |args| PatternArgs { args }.into()).await.into()
                }
                super::Index(ind) => {
                    args_index(CONTEXT, ind, &self.args).await
                }
                v => traits::bind_error(CONTEXT, src.with(v)).into()
            }
        }
    }
}
