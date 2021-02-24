//! Type-related functions, including type creation.

use abi_stable::std_types::{RArc, RString};
use ergo_runtime::{ergo_function, metadata::Doc, namespace_id, traits, types, ContextExt, Source};
use futures::FutureExt;
use grease::{
    depends, grease_trait_impl, match_value,
    type_erase::Erased,
    types::{GreaseType, Type},
    value::TypedValue,
    Value,
};

pub fn module() -> Value {
    crate::grease_string_map! {
        "new" = new_fn(),
        "Any" = match_any(),
        "Unset" = match_unset(),
        "Unit" = match_unit(),
        "Bool" = match_bool(),
        "String" = match_string(),
        "Path" = match_path(),
        "Map" = match_map(),
        "MapEntry" = match_map_entry(),
        "Array" = match_array(),
        "Function" = match_function(),
        "Iter" = match_iter()
    }
}

#[derive(abi_stable::StableAbi)]
#[repr(C)]
struct ScriptTypeData {
    inner_value: Value,
    bind_impl: Source<Value>,
}

fn new_fn() -> Value {
    ergo_function!(independent std::type::new,
        "Create a new type.

Arguments: (String id) (Function compose)
Keyword Arguments: (Optional (Function bind))

* `id` is used to derive the type identity, and should be unique to this type.
* `compose` should be a value which can be called in expressions and patterns; its call
implementations will be used by the returned value to compose and decompose values.
* `bind`, if provided, should be a value which will be bound to the inner value (as returned by a
call to `compose`) and will then be bound whenever instances of the new type are bound. If `bind`
is not provided, binding to instances of the new type will behave as if the value returned by
`compose` were bound (i.e. as if `bind` were specified as `:a -> :a`).

Returns a value which matches values of the new type (by application). If applied to no arguments,
returns a function which will compose or decompose values of the type (using `compose`
internally).

### Example Usage
Create type:
```
MyType = type:new \"library:MyType\" <| match:value ^[
    fn :a :b :c -> {a,b,c} # store internally as a map
    pat :out-a :out-b :out-c -> {a,b,c} -> {
        !:out-a = :a
        !:out-b = :b
        !:out-c = :c
    }
]
```

Create (compose) a value:
```
my-type = MyType: 1 2 3
```

Match a value:
```
MyType :m = :my-type
```

Match a value (without binding):
```
MyType _ = :my-type
!:MyType = :my-type
```

Ensure a value is `MyType`:
```
:m = MyType :my-type
```

Decompose a value (binding inner values):
```
MyType: :first :second :third = :my-type
```

Decompose a value (matching inner vaules):
```
MyType: 1 2 3 = :my-type
```

Bind to a value (accessing inner value):
```
my-type:a
```
",
    |ctx, args| {
        let id = args.next().ok_or("missing id")?;
        let interface = args.next().ok_or("missing composition function")?;
        let bind_impl = args.kw("bind");
        args.unused_arguments()?;

        let id = ctx.source_value_as::<types::String>(id).await?.unwrap();
        let id = id.await?.owned().0;

        let interface_doc = Doc::get(ctx, &*interface);
        let interface_doc = interface_doc.await?;

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
        ctx.traits.add_impl::<traits::Bind>(tp.clone(), {
            grease_trait_impl! {
                impl traits::Bind for _ {
                    async fn bind(&self, ctx: &mut ergo_runtime::Runtime, arg: Source<Value>) -> Value {
                        let data = unsafe { self.as_ref::<ScriptTypeData>() };
                        traits::bind(ctx, data.bind_impl.clone(), arg).await.map(|v| v.unwrap())?
                    }
                }
            }
        });
        let deps = depends![tp];
        let ret_tp = tp.clone();
        let construct: Value = types::Unbound::new(move |ctx, arg| {
            let interface = interface.clone();
            let tp = tp.clone();
            let bind_impl = bind_impl.clone();
            async move {
                let (arg_source, arg) = arg.take();
                match_value!(arg => {
                    types::Args => |args| {
                        let result = traits::bind(ctx, interface, arg_source.with(args.into())).await?;
                        let bind_impl = match bind_impl {
                            None => result.clone(),
                            Some(v) => traits::bind(ctx, v, result.clone()).await?
                        };
                        let inner_value = result.unwrap();
                        let data = ScriptTypeData { inner_value, bind_impl };
                        let deps = depends![data.inner_value];
                        unsafe {
                            Value::new(RArc::new(tp), async move {
                                Ok(RArc::new(Erased::new::<ScriptTypeData>(data)))
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
                                let (arg_source, arg) = ctx.source_pattern_value_as_type(arg, tp).await?.take();
                                let data = arg.await?;
                                let data = unsafe { (*data).as_ref::<ScriptTypeData>() };
                                traits::bind(ctx, to_bind, arg_source.with(data.inner_value.clone())).await.map(|v| v.unwrap())
                            }.boxed()
                        }, deps, None).into()
                    },
                    => |v| traits::bind_error(ctx, arg_source.with(v)).await?
                }).await
            }.boxed()
        }, deps, None).into();
        make_match_fn(ret_tp, Ok(construct), format!("Match a {} value.

If called with no arguments, returns a composition function:
{}", id, interface_doc))
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

fn make_match_fn<S: Into<types::String>>(
    tp: Type,
    constructor: ergo_runtime::Result<Value>,
    doc: S,
) -> Value {
    let deps = depends![namespace_id!(type), tp];
    types::Unbound::new(
        move |ctx, arg| {
            let tp = tp.clone();
            let constructor = constructor.clone();
            async move {
                let (arg_source, arg) = arg.take();
                match_value!(peek arg => {
                    types::Args => |args| {
                        let mut args = args.await?.owned().args;
                        let arg = args.next();
                        args.unused_arguments()?;
                        match arg {
                            None => constructor?,
                            Some(v) => ctx.source_value_as_type(v, tp).await?.unwrap(),
                        }
                    },
                    types::PatternArgs => |bind_args| {
                        let mut args = bind_args.await?.owned().args;
                        let arg = args.next();
                        args.unused_arguments()?;
                        match arg {
                            None => constructor?,
                            Some(v) => {
                                let deps = depends![*v];
                                types::Unbound::new(move |ctx, arg| {
                                    let v = v.clone();
                                    let tp = tp.clone();
                                    async move {
                                        let arg = ctx.source_pattern_value_as_type(arg, tp).await?;
                                        traits::bind(ctx, v, arg).await.map(|v| v.unwrap())
                                    }.boxed()
                                }, deps, None).into()
                            }
                        }
                    },
                    => |v| {
                        if let Some(Ok(t)) = v.peek_type() {
                            if t == &tp {
                                return Ok(v);
                            }
                        }
                        traits::bind_error(ctx, arg_source.with(v)).await?
                    }
                })
                .await
            }
            .boxed()
        },
        deps,
        Some(TypedValue::constant(doc.into())),
    )
    .into()
}

fn match_unset() -> Value {
    make_match_fn(
        types::Unset::grease_type(),
        Ok(types::Unset::new().into()),
        "Matches an Unset value.",
    )
}

fn match_unit() -> Value {
    make_match_fn(
        types::Unit::grease_type(),
        Err("cannot compose; use script syntax".into()),
        "Matches a Unit value.",
    )
}

fn match_bool() -> Value {
    make_match_fn(
        types::Bool::grease_type(),
        Err("cannot compose; use true and false indices".into()),
        "Matches a Bool value.",
    )
}

fn match_string() -> Value {
    make_match_fn(
        types::String::grease_type(),
        Err("cannot compose; use script syntax".into()),
        "Matches a String value.",
    )
}

fn match_map() -> Value {
    make_match_fn(
        types::Map::grease_type(),
        Err("cannot compose; use script syntax".into()),
        "Matches a Map value.",
    )
}

fn match_map_entry() -> Value {
    let construct: Value =
        types::Unbound::new(
            |ctx, arg| {
                async move {
                    let (arg_source, arg) = arg.take();
                    match_value!(arg => {
                        types::Args => |args| {
                            let mut args = args.await?.owned().args;
                            let key = args.next().ok_or("no key")?.unwrap();
                            let value = args.next().ok_or("no value")?.unwrap();
                            args.unused_arguments()?;
                            types::MapEntry { key, value }.into()
                        },
                        types::PatternArgs => |bind_args| {
                            let mut args = bind_args.await?.owned().args;
                            let key = args.next().ok_or("no key pattern")?;
                            let value = args.next().ok_or("no value pattern")?;
                            args.unused_arguments()?;
                            let deps = depends![*key, *value];
                            types::Unbound::new(move |ctx, arg| {
                                let key = key.clone();
                                let value = value.clone();
                                async move {
                                    let (entry_source, entry) = ctx.source_pattern_value_as::<types::MapEntry>(arg).await?.take();
                                    let entry = entry.await?.owned();
                                    traits::bind(ctx, key, entry_source.clone().with(entry.key)).await?;
                                    traits::bind(ctx, value, entry_source.with(entry.value)).await?;
                                    Ok(types::Unit.into())
                                }.boxed()
                            }, deps, None).into()
                        },
                        => |v| traits::bind_error(ctx, arg_source.with(v)).await?
                    })
                    .await
                }
                .boxed()
            },
            depends![namespace_id!(std::type::MapEntry::construct)],
            None
        )
        .into();
    make_match_fn(
        types::MapEntry::grease_type(),
        Ok(construct),
        "Matches a MapEntry value.

If called with no arguments, evaluates to a MapEntry constructor which accepts a key and value
argument in a call or pattern call.",
    )
}

fn match_array() -> Value {
    make_match_fn(
        types::Array::grease_type(),
        Err("cannot compose; use script syntax".into()),
        "Matches an Array value.",
    )
}

fn match_iter() -> Value {
    make_match_fn(
        types::Iter::grease_type(),
        Err("cannot compose; convert to/from other types".into()),
        "Matches an Iter value.",
    )
}

fn match_path() -> Value {
    make_match_fn(
        grease::path::PathBuf::grease_type(),
        Err("cannot compose; convert to/from other types".into()),
        "Matches a Path value.",
    )
}

fn match_function() -> Value {
    make_match_fn(
        types::Unbound::grease_type(),
        Err("cannot compose; use script syntax".into()),
        "Matches a Function value.",
    )
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
            t.assert_success("self:type:Unset :x = self:type:Unset:");
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
        fn bool(t) {
            t.assert_success("self:type:Bool :b = self:bool:true");
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
        fn map_entry(t) {
            t.assert_script_success("self:type:MapEntry: k v");
            t.assert_eq("(self:type:MapEntry: k v):key","k");
            t.assert_eq("(self:type:MapEntry: k v):value","v");
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

    // omit map/array/path/etc, they use the same macro so should be fundamentally the same

    ergo_script::test! {
        fn new(t) {
            t.assert_content_eq("my_type = self:type:new \"scoped:my_type\" (:a -> !self:match :a ^[
                fn (self:type:String :a) (self:type:Unit :b) -> [:a,:b]
                pat :a :b -> [:a',:b'] -> { !:a = :a'; !:b = :b' }
            ])
            :val = my_type: str ()
            !:my_type = :val
            my_type _ = :val
            my_type: :x :y = :val
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

    ergo_script::test! {
        fn new_bind(t) {
            t.assert_content_eq("my_type = self:type:new \"scoped:my_type\" (:a -> !self:match :a ^[
                fn (self:type:String :a) (self:type:Unit :b) -> [:a,:b]
                pat :a :b -> [:a',:b'] -> { !:a = :a'; !:b = :b' }
            ])
            :val = my_type: str ()
            val:0",
                "str"
            );
            t.assert_content_eq("my_type_bind = [:a,:b] -> index string -> :a
            my_type = self:type:new (bind = :my_type_bind) \"scoped:my_type\" (:a -> !self:match :a ^[
                fn (self:type:String :a) (self:type:Unit :b) -> [:a,:b]
                pat :a :b -> [:a',:b'] -> { !:a = :a'; !:b = :b' }
            ])
            :val = my_type: str ()
            val:string",
                "str"
            );
        }
    }
}
