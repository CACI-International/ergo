//! Trait-related functions.

use ergo_runtime::Value;
use ergo_runtime::{abi_stable::type_erase::Erased, type_system::Trait, types, Context};

pub fn module() -> Value {
    crate::make_string_map! {
        "Bind" = bind::module(),
        "Display" = display::module(),
        "Into" = into::module(),
        "Stored" = stored::module(),
        "new" = new()
    }
}

#[types::ergo_fn]
/// Create a new trait.
///
/// Arguments: `(String :id)`
///
/// `id` is used to derive the trait identity, and must be unique to this trait.
///
/// Returns a map with two functions:
/// * `impl` - a Function which takes a value and a Type, and implements the trait for Type with
/// the passed value as the trait data (essentially associating a value with the Type, keyed by the
/// trait).
/// * `get` - a Function which takes a value and returns the implemented trait data for that value's
/// Type, or Unset if the trait is unimplemented.
///
/// It is recommended that you wrap or hide these functions to provide a better interface that
/// checks types and is ergonomic.
async fn new(id: types::String) -> Value {
    let trt = Trait::named(id.as_ref().as_str().as_bytes());

    let imp = {
        let trt = trt.clone();
        types::ergo_fn_value! {
            #[cloning(trt)]
            #[depends(trt)]
            /// Implement the trait.
            ///
            /// Arguments: `:value (Type :type)`
            ///
            /// Returns the passed `type` for convenience.
            async fn r#impl(value: _, r#type: types::Type) -> Value {
                let tp = r#type.as_ref().tp.clone();
                unsafe {
                    Context::global()
                        .traits
                        .add_impl_unsafe(trt, tp, Erased::new::<Value>(value));
                }

                r#type.into()
            }
        }
    };

    let get = types::ergo_fn_value! {
        #[cloning(trt)]
        #[depends(trt)]
        /// Get the trait implementation for the type of the given value.
        ///
        /// Arguments: `:value`
        ///
        /// Returns the trait implementation, or Unset if the trait is not implemented.
        async fn get(mut value: _) -> Value {
            Context::eval(&mut value).await?;
            match Context::global()
                .traits
                .get_impl(value.ergo_type().unwrap(), &trt)
            {
                None => types::Unset.into(),
                Some(v) => unsafe { v.as_ref().as_ref::<Value>() }.clone(),
            }
        }
    };

    crate::make_string_map! {
        "impl" = imp,
        "get" = get
    }
}

mod bind {
    use ergo_runtime::{
        abi_stable::type_erase::Erased, metadata::Source, traits, type_system::ergo_trait_impl,
        types, Context, Value,
    };

    pub fn module() -> Value {
        crate::make_string_map! {
            "impl" = r#impl()
        }
    }

    #[types::ergo_fn]
    /// Implement the Bind trait for a type.
    ///
    /// Arguments: `(Function :bind) (Type :type)`
    ///
    /// `bind` should be a function which is passed two arguments: the target `type`-typed value,
    /// and the value being bound. It should return the result of binding.
    ///
    /// The Bind trait is used when a value is called as a function, indexed, or bound with `=`.
    ///
    /// Returns the passed `type` for convenience (most useful to pipe through multiple
    /// implementations).
    async fn r#impl(bind: types::Unbound, r#type: types::Type) -> Value {
        let tp = r#type.as_ref().tp.clone();
        Context::global().traits.add_impl::<traits::Bind>(tp, {
            let mut imp = ergo_trait_impl! {
                impl traits::Bind for _ {
                    async fn bind(&self, arg: Value) -> Value {
                        let imp = unsafe { TRAIT_DATA.as_ref::<Value>() }.clone();
                        traits::bind(imp, Source::imbue(Source::get(&arg).with(types::Args {
                            args: types::args::Arguments::positional(vec![SELF_VALUE.clone(), arg]).unchecked()
                        }.into()))).await
                    }
                }
            };
            imp.ergo_trait_data = Erased::new::<Value>(bind.into());
            imp
        });
        r#type.into()
    }
}

mod display {
    use ergo_runtime::{
        abi_stable::type_erase::Erased, metadata::Source, traits, type_system::ergo_trait_impl,
        types, Context, Value,
    };

    pub fn module() -> Value {
        crate::make_string_map! {
            "impl" = r#impl()
        }
    }

    #[types::ergo_fn]
    /// Implement the Display trait for a type.
    ///
    /// Arguments: `(Function :display) (Type :type)`
    ///
    /// `display` should be a function which is passed the target `type`-typed value. It should
    /// return a value which will then be displayed (such as a String).
    ///
    /// The Display trait is used when formatting into a string, such as for `value` in `"$value",
    /// when using logging functions, etc.
    ///
    /// Returns the passed `type` for convenience (most useful to pipe through multiple
    /// implementations).
    async fn r#impl(display: types::Unbound, r#type: types::Type) -> Value {
        let tp = r#type.as_ref().tp.clone();
        Context::global().traits.add_impl::<traits::Display>(tp, {
            let mut imp = ergo_trait_impl! {
                impl traits::Display for _ {
                    async fn fmt(&self, f: &mut traits::Formatter) -> ergo_runtime::RResult<()> {
                        let imp = unsafe { TRAIT_DATA.as_ref::<Value>() }.clone();
                        let to_display = traits::bind(imp, Source::imbue(Source::get(&SELF_VALUE).with(types::Args {
                            args: types::args::Arguments::positional(vec![SELF_VALUE.clone()]).unchecked()
                        }.into()))).await;
                        traits::display(to_display, f).await.into()
                    }
                }
            };
            imp.ergo_trait_data = Erased::new::<Value>(display.into());
            imp
        });
        r#type.into()
    }
}

mod stored {
    use ergo_runtime::{
        abi_stable::{type_erase::Erased, StableAbi},
        metadata::Source,
        traits,
        type_system::{ergo_trait_impl, Type},
        types, Context, Value,
    };

    pub fn module() -> Value {
        crate::make_string_map! {
            "impl" = r#impl()
        }
    }

    #[derive(StableAbi)]
    #[repr(C)]
    struct TraitData {
        pub put: Value,
        pub get: Value,
        pub tp: Type,
    }

    #[types::ergo_fn]
    /// Implement the Stored trait for a type.
    ///
    /// Arguments: `(Function :put) (Function :get) (Type :type)`
    ///
    /// `put` should be a function which is passed the target `type`-typed value and should return
    /// a value to be stored (that isn't `type`-typed).
    ///
    /// `get` should be a function which is passed the stored value and should return a
    /// `type`-typed value.
    ///
    /// The Stored trait is used when a value is persisted.
    ///
    /// It is _imperative_ that this function is only called on types created by `std:Type:new`,
    /// otherwise serialization will not occur correctly and will likely cause a crash.
    ///
    /// Returns the passed `type` for convenience (most useful to pipe through multiple
    /// implementations).
    async fn r#impl(put: types::Unbound, get: types::Unbound, r#type: types::Type) -> Value {
        let tp = r#type.as_ref().tp.clone();
        Context::global().traits.add_impl::<traits::Stored>(tp.clone(), {
            let mut imp = ergo_trait_impl! {
                impl traits::Stored for _ {
                    async fn put(&self, data: &mut traits::PutData<'_>) -> ergo_runtime::RResult<()> {
                        ergo_runtime::error_info!(
                            labels: [
                                primary(Source::get(SELF_VALUE).with("while storing this value"))
                            ],
                            async {
                                let imp = unsafe { TRAIT_DATA.as_ref::<TraitData>() }.put.clone();
                                let to_put = traits::bind(imp, Source::imbue(Source::get(&SELF_VALUE).with(types::Args {
                                    args: types::args::Arguments::positional(vec![SELF_VALUE.clone()]).unchecked()
                                }.into()))).await;
                                let id = to_put.id().await;
                                data.write_value(to_put).await?;
                                bincode::serialize_into(data, &id)
                            }
                        ).into()
                    }

                    async fn get(mut data: &mut traits::GetData<'_>) -> ergo_runtime::RResult<Erased> {
                        ergo_runtime::error_info!(
                            async {
                                let id: u128 = bincode::deserialize_from(&mut data)?;
                                let to_get = data.read_value(id).await?;
                                let trait_data = unsafe { TRAIT_DATA.as_ref::<TraitData>() };
                                let mut value = traits::bind(trait_data.get.clone(), types::Args {
                                    args: types::args::Arguments::positional(vec![to_get]).unchecked()
                                }.into()).await;
                                Context::eval(&mut value).await?;
                                if value.ergo_type().unwrap() != &trait_data.tp {
                                    return Err(traits::type_error_for_t(value, &trait_data.tp).into_error().into());
                                }
                                let data = value.data().unwrap();
                                // XXX this only works for script-created types!
                                let data = unsafe { (*data).as_ref::<Value>() }.clone();
                                ergo_runtime::Result::Ok(Erased::new::<Value>(data))
                            }
                        ).into()
                    }
                }
            };
            imp.ergo_trait_data = Erased::new::<TraitData>(TraitData {
                put: put.into(),
                get: get.into(),
                tp
            });
            imp
        });
        r#type.into()
    }
}

mod into {
    use ergo_runtime::{
        abi_stable::{closure::FnPtr, future::BoxFuture, type_erase::Erased},
        depends,
        metadata::Source,
        traits, types, Context, Value,
    };
    use futures::FutureExt;

    pub fn module() -> Value {
        crate::make_string_map! {
            "impl" = r#impl(),
            "into" = into()
        }
    }

    #[types::ergo_fn]
    /// Implement the Into trait for a type.
    ///
    /// Arguments: `(Type :into_type) (Function :into) (Type :type)`
    ///
    /// `into` should be a function which is passed the target `type`-typed value. It should return
    /// a value with type `into_type`.
    ///
    /// The Into trait is used by various functions to convert values.
    ///
    /// Returns the passed `type` for convenience (most useful to pipe through multiple
    /// implementations).
    async fn r#impl(into_type: types::Type, into: types::Unbound, r#type: types::Type) -> Value {
        let tp = r#type.as_ref().tp.clone();
        let into_tp = into_type.as_ref().tp.clone();
        let trt = traits::into_trait(into_tp);

        extern "C" fn into_f<'a>(data: &'a Erased, v: Value) -> BoxFuture<'a, Value> {
            BoxFuture::new(async move {
                let imp = unsafe { data.as_ref::<Value>() }.clone();
                traits::bind(
                    imp,
                    Source::imbue(
                        Source::get(&v).with(
                            types::Args {
                                args: types::args::Arguments::positional(vec![v]).unchecked(),
                            }
                            .into(),
                        ),
                    ),
                )
                .await
            })
        }
        let imp = Erased::new(traits::IntoTypedImpl::<()> {
            into_typed: unsafe { FnPtr::new(into_f) },
            ergo_trait_data: Erased::new::<Value>(into.into()),
            _phantom0: Default::default(),
        });
        unsafe {
            Context::global().traits.add_impl_unsafe(trt, tp, imp);
        }

        r#type.into()
    }

    #[types::ergo_fn]
    /// Return a matching function to convert values to the given type.
    ///
    /// Arguments: `(Type :type)`
    ///
    /// ## Example Usage
    /// ```ergo
    /// (into $String) :x = ...
    /// ```
    async fn into(into_type: types::Type) -> Value {
        let into_type = into_type.as_ref().tp.clone();
        types::ergo_fn_value! {
            #[depends(into_type)]
            #[cloning(into_type)]
            async fn into(target: _) -> Value {
                let deps = CALL_DEPENDS + depends![dyn target];
                types::Unbound::new_no_doc(
                    move |arg| {
                        let target = target.clone();
                        let into_type = into_type.clone();
                        async move {
                            let v = ergo_runtime::try_result!(
                                traits::into_for(&into_type, arg).await
                            );
                            traits::bind(target, v).await
                        }
                        .boxed()
                    },
                    deps,
                )
                .into()
            }
        }
    }
}

#[cfg(test)]
mod test {
    ergo_script::tests! {
        fn bind_impl(t) {
            t.assert_eq("MyType = {
                Self = self:Type:new MyType
                $Self | self:trait:Bind:impl <| fn (Self:@ :a :b) :x -> self:match $x [
                    index a -> $a
                    index b -> $b
                ]
            }
            inst = MyType:new 1 2
            inst:a",
                "1"
            );
        }

        fn display_impl(t) {
            t.assert_eq("MyType = {
                Self = self:Type:new MyType
                $Self | self:trait:Display:impl <| fn (Self:@ :a :b) -> \"MyType a=$a b=$b\"
            }
            inst = MyType:new 1 2
            \"$inst\"",
                "\"MyType a=1 b=2\""
            );
        }

        fn into_impl(t) {
            t.assert_eq("MyType = {
                Self = self:Type:new MyType
                $Self | self:trait:Into:impl self:Number <| fn (Self _) -> self:Number:new 42
            }
            inst = MyType:new 1 2
            self:trait:Into:into self:Number |> :x = $inst
            $x",
                "self:Number:new 42"
            );
        }

        fn new(t) {
            t.assert_eq("Color = self:trait:new Color
            Color:impl blue self:String
            Color:impl orange self:Array
            self:Type:any :a = Color:get abcde
            self:Type:any :b = Color:get []
            self:Type:any :c = Color:get {}
            [$a,$b,$c]",
                "[blue, orange, $unset]"
            );
        }
    }
}
