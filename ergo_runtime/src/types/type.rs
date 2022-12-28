//! The Type type.

use crate as ergo_runtime;
use crate::abi_stable::{future, u128::U128, StableAbi};
use crate::metadata::Source;
use crate::traits;
use crate::type_system::{ergo_traits_fn, ErgoType};
use crate::{TypedValue, Value};

/// Script Type type.
///
/// Types support an instance-associated value to use when an `Index` is bound.
#[derive(Clone, Debug, ErgoType, StableAbi)]
#[repr(C)]
pub struct Type {
    pub tp: crate::type_system::Type,
    pub index: Value,
}

impl crate::value::ValueDataInterface for Type {
    /// Get the identity of a value.
    fn id(&self) -> future::BoxFuture<crate::value::IdInfo<U128>> {
        future::BoxFuture::new(async move {
            crate::depends![dyn Type::ergo_type(), self.tp, self.index]
                .id()
                .await
                .into()
        })
    }

    /// Provide late bindings to a value.
    fn late_bind(&mut self, context: &mut crate::value::LateBindContext) {
        self.index.late_bind(context);
    }

    /// Get the set of late bindings in a value.
    fn late_bound(&self) -> crate::value::LateBound {
        ergo_runtime::value::LateBind::late_bound(&self.index)
    }

    fn gc_refs(&self, v: &mut crate::gc::Visitor) {
        crate::gc::GcRefs::gc_refs(&self.index, v);
    }

    /// Get the value data.
    fn get(&self) -> crate::value::ValueType {
        crate::value::ValueType::typed(self)
    }
}

impl From<Type> for TypedValue<Type> {
    fn from(v: Type) -> Self {
        Value::new(v).as_type::<Type>().unwrap()
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

impl Type {
    pub fn basic(tp: crate::type_system::Type) -> Self {
        Type {
            tp,
            index: crate::types::Map(Default::default()).into(),
        }
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
                    let tp = self.tp.clone();
                    crate::types::unbound_value! {
                        #![contains(v)]
                        return crate::error_info! {
                            labels: [
                                secondary(source.with("while binding here"))
                            ],
                            async {
                                drop(crate::Context::eval(&mut ARG).await);
                                if ARG.ergo_type().unwrap() != tp {
                                    Err(traits::type_error_for_t(ARG, &tp))
                                } else {
                                    Ok(traits::bind(v, ARG).await)
                                }
                            }
                        };
                    }
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
                v => traits::bind_error(&SELF_VALUE, &v).into()
            }
        }
    }
}
