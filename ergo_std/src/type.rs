//! Type-related functions, including type creation.

use abi_stable::std_types::{RArc, RString};
use ergo_runtime::{ergo_function, namespace_id, traits, types, ContextExt};
use futures::FutureExt;
use grease::{
    depends, grease_trait_impl, match_value, type_erase::Erased, types::GreaseType,
    value::TypedValue, Value,
};

pub fn module() -> Value {
    crate::grease_string_map! {
        "A map of type-related functions:"
        "new": "Create a new type." = new_fn(),
        "Any": "Match any type." = match_any(),
        "Unset": "Match the unset type (returned by unset values)." = match_unset(),
        "Unit": "Match the unit type." = match_unit(),
        "String": "Match the string type." = match_string(),
        "Path": "Match the path type." = match_path(),
        "Map": "Match the map type." = match_map(),
        "Array": "Match the array type." = match_array(),
        "Function": "Match the function type." = match_function()
    }
}

fn new_fn() -> Value {
    ergo_function!(independent std::type::new,
        "Create a new type.

Arguments: <String> <interface>
The String is used to derive the type identity, and should be unique to this type.
`interface` should be a value which can be called in expressions and bindings; its bind
implementation will be used by the returned value to structure and destructure values.

Returns a value which can be used to create values of the new type (by application) or decompose
values of the new type (by application in a binding). The returned value should be applied to
values in the same manner that `interface` supports.",
    |ctx, args| {
        let id = args.next().ok_or("missing id")?;
        let interface = args.next().ok_or("missing interface")?;
        args.unused_arguments()?;

        let id = ctx.source_value_as::<types::String>(id).await?.unwrap();
        let id = id.await?.owned().0;

        let mut tp = grease::types::Type::named(id.as_str().as_bytes());
        // Make type depend on interface identity, so if the interface changes the type will be
        // considered a new type. This may be very relevant if/when support for storing custom
        // types is added.
        tp.data = grease::type_erase::ErasedTrivial::new(interface.id());
        ctx.traits.add_impl::<traits::TypeName>(tp.clone(), {
            let mut imp = grease_trait_impl! {
                impl traits::TypeName for _ {
                    async fn type_name() -> RString {
                        unsafe { TRAIT_DATA.as_ref::<RString>() }.clone()
                    }
                }
            };
            imp.grease_trait_data = Erased::new::<RString>(id.clone());
            imp
        });
        let deps = depends![tp, *interface];
        types::Unbound::new(move |ctx, arg| {
            let interface = interface.clone();
            let tp = tp.clone();
            async move {
                let (arg_source, arg) = arg.take();
                match_value!(arg => {
                    types::Args => |args| {
                        let result = traits::bind(ctx, interface, arg_source.with(args.into())).await?.unwrap();
                        let deps = depends![result];
                        unsafe {
                            Value::new(RArc::new(tp), async move {
                                Ok(RArc::new(Erased::new::<Value>(result)))
                            }, deps)
                        }
                    },
                    types::PatternArgs => |bind_args| {
                        let to_bind = traits::bind(ctx, interface, arg_source.with(bind_args.into())).await?;
                        let deps = depends![tp, *to_bind];
                        types::Unbound::new(move |ctx, arg| {
                            let to_bind = to_bind.clone();
                            let tp = tp.clone();
                            async move {
                                let (arg_source, arg) = arg.take();
                                if arg.grease_type().await? != &tp {
                                    return traits::bind_error(ctx, arg_source.with(arg)).await;
                                }
                                let data = arg.await?;
                                let val = unsafe { (*data).as_ref::<Value>() }.clone();
                                traits::bind(ctx, to_bind, arg_source.with(val)).await.map(|v| v.unwrap())
                            }.boxed()
                        }, deps, None).into()
                    },
                    => |v| {
                        if v.grease_type().await? == &tp {
                            v
                        } else {
                            traits::bind_error(ctx, arg_source.with(v)).await?
                        }
                    }
                }).await
            }.boxed()
        }, deps, None).into()
    })
    .into()
}

fn match_any() -> Value {
    types::Unbound::new(
        |_ctx, arg| {
            async move {
                let arg = arg.unwrap();
                match_value!(arg => {
                    types::Args => |args| {
                        let mut args = args.await?.owned().args;
                        let v = args.next().ok_or("no value")?;
                        args.unused_arguments()?;
                        v.unwrap()
                    },
                    types::PatternArgs => |bind_args| {
                        let mut args = bind_args.await?.owned().args;
                        let v = args.next().ok_or("no value")?;
                        args.unused_arguments()?;
                        let deps = depends![*v];
                        types::Unbound::new(move |ctx, arg| {
                            let v = v.clone();
                            async move {
                                traits::bind(ctx, v, arg).await.map(|v| v.unwrap())
                            }.boxed()
                        }, deps, None).into()
                    },
                    => |v| v
                })
                .await
            }
            .boxed()
        },
        depends![namespace_id!(std::type::Unit)],
        Some(TypedValue::constant("Matches any value.".into())),
    )
    .into()
}

macro_rules! simple_match_fn {
    ( $t:ty, $( $p:ident )::+, $doc:literal ) => {
        types::Unbound::new(
            |ctx, arg| {
                async move {
                    let (arg_source, arg) = arg.take();
                    match_value!(arg => {
                        types::Args => |args| {
                            let mut args = args.await?.owned().args;
                            let v = args.next().ok_or("no value")?;
                            args.unused_arguments()?;
                            ctx.source_pattern_value_as::<$t>(v).await?.unwrap().into()
                        },
                        types::PatternArgs => |bind_args| {
                            let mut args = bind_args.await?.owned().args;
                            let v = args.next().ok_or("no value")?;
                            args.unused_arguments()?;
                            let deps = depends![*v];
                            types::Unbound::new(move |ctx, arg| {
                                let v = v.clone();
                                async move {
                                    if arg.grease_type().await? != &<$t>::grease_type() {
                                        traits::bind_error(ctx, arg).await
                                    } else {
                                        traits::bind(ctx, v, arg).await.map(|v| v.unwrap())
                                    }
                                }.boxed()
                            }, deps, None).into()
                        },
                        $t => |v| v.into(),
                        => |v| traits::bind_error(ctx, arg_source.with(v)).await?
                    })
                    .await
                }
                .boxed()
            },
            depends![namespace_id!($($p)::+)],
            Some(TypedValue::constant($doc.into())),
        )
        .into()
    }
}

fn match_unset() -> Value {
    simple_match_fn!(types::Unset, std::type::Unset, "Matches an Unset value.")
}

fn match_unit() -> Value {
    simple_match_fn!(types::Unit, std::type::Unit, "Matches a Unit value.")
}

fn match_string() -> Value {
    simple_match_fn!(types::String, std::type::String, "Matches a String value.")
}

fn match_map() -> Value {
    simple_match_fn!(types::Map, std::type::Map, "Matches a Map value.")
}

fn match_array() -> Value {
    simple_match_fn!(types::Array, std::type::Array, "Matches an Array value.")
}

fn match_path() -> Value {
    simple_match_fn!(grease::path::PathBuf, std::type::Path, "Matches a Path value.")
}

fn match_function() -> Value {
    simple_match_fn!(types::Unbound, std::type::Function, "Matches a Function value.")
}

#[cfg(test)]
mod test {
    ergo_script::test! {
        fn any(t) {
            t.assert_eq("self:type:Any hello", "hello");
            t.assert_eq("self:type:Any :x = hello; :x", "hello");
            t.assert_success("!self:type:Any = hello");
        }
    }

    ergo_script::test! {
        fn unset(t) {
            t.assert_script_success("self:type:Unset {}:key");
            t.assert_script_fail("self:type:Unset str");
            t.assert_success("self:type:Unset :x = {}:key");
            t.assert_success("!self:type:Unset = {}:key");
        }
    }

    ergo_script::test! {
        fn unit(t) {
            t.assert_eq("self:type:Unit ()", "()");
            t.assert_script_fail("self:type:Unit str");
            t.assert_eq("self:type:Unit :x = (); :x", "()");
            t.assert_script_fail("self:type:Unit :x = str");
            t.assert_success("!self:type:Unit = ()");
            t.assert_script_fail("!self:type:Unit = str");
        }
    }

    ergo_script::test! {
        fn string(t) {
            t.assert_eq("self:type:String hi", "hi");
            t.assert_script_fail("self:type:String ()");
            t.assert_eq("self:type:String :x = hi; :x", "hi");
            t.assert_script_fail("self:type:String :x = ()");
            t.assert_success("!self:type:String = hi");
            t.assert_script_fail("!self:type:String = ()");
        }
    }

    ergo_script::test! {
        fn function(t) {
            t.assert_success("self:type:Function (:a -> :a)");
            t.assert_script_fail("self:type:Function ()");
            t.assert_success("self:type:Function :x = :a -> :a; :x");
            t.assert_script_fail("self:type:Function :x = ()");
            t.assert_success("!self:type:Function = :a -> :a");
            t.assert_script_fail("!self:type:Function = ()");
        }
    }

    // omit map/array/path, they use the same macro so should be fundamentally the same

    ergo_script::test! {
        fn new(t) {
            t.assert_content_eq(":my_type = self:type:new \"scoped:my_type\" (:a -> !self:match :a ^[
                fn (self:type:String :a) (self:type:Unit :b) -> [:a,:b]
                pat :a :b -> [:a',:b'] -> { !:a = :a'; !:b = :b' }
            ])
            :val = my_type str ()
            !:my_type = :val
            my_type :x :y = :val
            [:x,:y]",
                "[str,()]"
            );
            t.assert_script_fail(":my_type = self:type:new \"scoped:my_type\" (:a -> !self:match :a ^[
                fn (self:type:String :a) (self:type:Unit :b) -> [:a,:b]
                pat :a :b -> [:a',:b'] -> { !:a = :a'; !:b = :b' }
            ])
            !:my_type = ()");
        }
    }
}
