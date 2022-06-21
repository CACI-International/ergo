//! The Type type.

use crate as ergo_runtime;
use crate::abi_stable::StableAbi;
use crate::metadata::Source;
use crate::traits;
use crate::type_system::{ergo_traits_fn, ErgoType};
use crate::{depends, TypedValue, Value};
use futures::FutureExt;

/// Script Type type.
///
/// Types support an instance-associated value to use when an `Index` is bound.
#[derive(Clone, Debug, ErgoType, StableAbi)]
#[repr(C)]
pub struct Type {
    pub tp: crate::type_system::Type,
    pub index: Value,
}

// TODO: include `index` in the identity? This will cause problems if trying to compare types for
// equality.

impl std::hash::Hash for Type {
    fn hash<H: std::hash::Hasher>(&self, h: &mut H) {
        self.tp.hash(h);
    }
}

impl From<Type> for TypedValue<Type> {
    fn from(v: Type) -> Self {
        Self::constant(v)
    }
}

impl traits::NestedValues for Type {
    fn nested_values(&self) -> Vec<&Value> {
        vec![&self.index]
    }
    fn nested_values_mut(&mut self) -> Vec<&mut Value> {
        vec![&mut self.index]
    }
}

ergo_traits_fn! {
    crate::ergo_type_name!(traits, Type);
    traits::Nested::add_impl::<Type>(traits);

    impl traits::Display for Type {
        async fn fmt(&self, f: &mut traits::Formatter) -> crate::RResult<()> {
            crate::error_info!(
                labels: [
                    primary(Source::get(SELF_VALUE).with("while displaying this value"))
                ],
                async {
                    write!(f, "{} Type", traits::type_name_for(&self.tp))?;
                    crate::Result::Ok(())
                }
            ).into()
        }
    }

    impl traits::Bind for Type {
        async fn bind(&self, mut arg: Value) -> Value {
            let source = Source::get(&arg);
            crate::try_result!(crate::Context::eval(&mut arg).await);

            crate::value::match_value! { arg,
                ind@super::Index(_) => traits::bind(self.index.clone(), ind.into()).await,
                super::Args { mut args } => {
                    let v = crate::try_result!(args.next_or_error("binding", source));
                    crate::try_result!(args.unused_arguments());
                    let deps = depends![dyn v];
                    let tp = self.tp.clone();
                    super::Unbound::new_no_doc(move |mut arg| {
                        let v = v.clone();
                        let tp = tp.clone();
                        async move {
                            crate::error_info! {
                                labels: [
                                    secondary(source.with("while binding here"))
                                ],
                                async {
                                    drop(crate::Context::eval(&mut arg).await);
                                    if arg.ergo_type().unwrap() != tp {
                                        Err(traits::type_error_for_t(arg, &tp))
                                    } else {
                                        Ok(traits::bind(v, arg).await)
                                    }
                                }
                            }.into()
                        }.boxed()
                    }, deps).into()
                },
                Type { tp, .. } => {
                    if &self.tp == &tp {
                        super::Unit.into()
                    } else {
                        crate::error!(
                            labels: [
                                primary(source.with("while binding to this Type"))
                            ],
                            error: format!("type mismatch: expected {}, got {}", traits::type_name_for(&self.tp), traits::type_name_for(&tp))
                        ).into()
                    }
                },
                v => traits::bind_error(v).into()
            }
        }
    }
}
