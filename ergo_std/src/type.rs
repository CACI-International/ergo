//! Type-related functions, including type creation.

use ergo_runtime::abi_stable::{
    std_types::{RArc, RString},
    type_erase::{Erased, ErasedTrivial},
    StableAbi,
};
use ergo_runtime::{
    depends,
    metadata::{Doc, Source},
    nsid, traits, try_result,
    type_system::{ergo_trait_impl, ErgoType, Type},
    types,
    value::match_value,
    Context, Value,
};
use futures::FutureExt;

pub fn module() -> Value {
    crate::make_string_map! {
        "new" = new(),
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
        "Iter" = match_iter(),
        "Number" = match_number(),
        "Error" = match_error()
    }
}

#[derive(StableAbi)]
#[repr(C)]
struct ScriptTypeData {
    inner_value: Value,
    bind_impl: Value,
}

#[types::ergo_fn]
/// Create a new type.
///
/// Arguments: `(String :id) (Function :compose)`
/// * `id` is used to derive the type identity, and should be unique to this type.
/// * `compose` should be a value which can be called in expressions and patterns; its call
/// implementations will be used by the returned value to compose and decompose values.
///
/// Keyed Arguments:
/// * `Function :bind`: a value which will be bound to the inner value (as returned by a
///   call to `compose`) and will then be bound whenever instances of the new type are bound. If `bind`
///   is not provided, binding to instances of the new type will behave as if the value returned by
///   `compose` were bound (i.e. as if `bind` were specified as `:a -> :a`).
///
/// Returns a value which matches values of the new type (by application). If applied to no arguments,
/// returns a function which will compose or decompose values of the type (using `compose`
/// internally).
///
/// ### Example Usage
/// Create type:
/// ```ergo
/// MyType = type:new \"library:MyType\" <| match:value ^[
///     fn :a :b :c -> {a,b,c} # store internally as a map
///     pat :out-a :out-b :out-c -> {a,b,c} -> {
///         !:out-a = :a
///         !:out-b = :b
///         !:out-c = :c
///     }
/// ]
/// ```
///
/// Create (compose) a value:
/// ```
/// my-type = MyType: 1 2 3
/// ```
///
/// Match a value:
/// ```
/// MyType :m = :my-type
/// ```
///
/// Match a value (without binding):
/// ```
/// MyType _ = :my-type
/// !:MyType = :my-type
/// ```
///
/// Ensure a value is `MyType`:
/// ```
/// :m = MyType :my-type
/// ```
///
/// Decompose a value (binding inner values):
/// ```
/// MyType: :first :second :third = :my-type
/// ```
///
/// Decompose a value (matching inner vaules):
/// ```
/// MyType: 1 2 3 = :my-type
/// ```
///
/// Bind to a value (accessing inner value):
/// ```
/// my-type:a
/// ```
async fn new(id: types::String, interface: _, (bind): [_]) -> Value {
    let interface_doc = try_result!(Doc::get(&interface).await);

    let mut tp = Type::named(id.as_ref().as_str().as_bytes());
    let id = id.to_owned().0;

    // Make type depend on interface identity, so if the interface changes the type will be
    // considered a new type. This may be very relevant if/when support for storing custom
    // types is added.
    tp.data = ErasedTrivial::new(interface.id());
    Context::global()
        .traits
        .add_impl::<traits::TypeName>(tp.clone(), {
            let mut imp = ergo_trait_impl! {
                impl traits::TypeName for _ {
                    fn type_name() -> RString {
                        unsafe { TRAIT_DATA.as_ref::<RString>() }.clone()
                    }
                }
            };
            imp.ergo_trait_data = Erased::new::<RString>(id.clone());
            imp
        });
    Context::global()
        .traits
        .add_impl::<traits::Bind>(tp.clone(), {
            ergo_trait_impl! {
                impl traits::Bind for _ {
                    async fn bind(&self, arg: Value) -> Value {
                        let data = unsafe { self.as_ref::<ScriptTypeData>() };
                        traits::bind(data.bind_impl.clone(), arg).await
                    }
                }
            }
        });

    let deps = depends![tp];
    let ret_tp = tp.clone();
    let construct: Value = types::Unbound::new_no_doc(move |arg| {
        let interface = interface.clone();
        let tp = tp.clone();
        let bind = bind.clone();
        async move {
            let arg_source = Source::get(&arg);
            match_value!{ arg,
                a@types::Args { .. } => {
                    let result = traits::bind(interface, Source::imbue(arg_source.with(a.into()))).await;
                    let bind_impl = match bind {
                        None => result.clone(),
                        Some(v) => traits::bind(v, result.clone()).await
                    };
                    let inner_value = result;
                    let data = ScriptTypeData { inner_value, bind_impl };
                    let deps = depends![data.inner_value];
                    unsafe {
                        Value::new(RArc::new(tp), RArc::new(Erased::new::<ScriptTypeData>(data)), deps)
                    }
                },
                a@types::PatternArgs { .. } => {
                    let to_bind = traits::bind(interface, Source::imbue(arg_source.with(a.into()))).await;
                    let deps = depends![tp, to_bind];
                    types::Unbound::new_no_doc(move |mut arg| {
                        let to_bind = to_bind.clone();
                        let tp = tp.clone();
                        async move {
                            try_result!(Context::eval(&mut arg).await);
                            if arg.ergo_type().unwrap() != &tp {
                                return traits::type_error_for_t(arg, &tp).into();
                            }
                            let data = arg.data().unwrap();
                            let data = unsafe { (*data).as_ref::<ScriptTypeData>() };
                            traits::bind(to_bind, data.inner_value.clone()).await
                        }.boxed()
                    }, deps).into()
                },
                v => traits::bind_error(v).into()
            }
        }.boxed()
    }, deps).into();
    make_match_fn(
        ret_tp,
        Ok(construct),
        format!(
            "Match a {} value.

If called with no arguments, returns a composition function:
{}",
            id, interface_doc
        ),
    )
}

fn match_any() -> Value {
    types::Unbound::new(
        |arg| {
            async move {
                match_value! {arg,
                    types::Args { mut args } => {
                        let v = try_result!(args.next().ok_or("no value"));
                        try_result!(args.unused_arguments());
                        v
                    },
                    types::PatternArgs { mut args } => {
                        let v = try_result!(args.next().ok_or("no value"));
                        try_result!(args.unused_arguments());
                        let deps = depends![v];
                        types::Unbound::new_no_doc(move |arg| {
                            let v = v.clone();
                            async move {
                                traits::bind(v, arg).await
                            }.boxed()
                        }, deps).into()
                    },
                    v => v
                }
            }
            .boxed()
        },
        depends![nsid!(std::type::Any)],
        "Matches any value.",
    )
    .into()
}

fn make_match_fn<S: Into<String>>(
    tp: Type,
    constructor: ergo_runtime::Result<Value>,
    doc: S,
) -> Value {
    let deps = depends![nsid!(type), tp];
    types::Unbound::new(
        move |arg| {
            let tp = tp.clone();
            let constructor = constructor.clone();
            async move {
                match_value! {arg,
                    types::Args { mut args } => {
                        let arg = args.next();
                        try_result!(args.unused_arguments());
                        match arg {
                            None => try_result!(constructor),
                            Some(mut v) => {
                                drop(Context::eval(&mut v).await);
                                if v.ergo_type().unwrap() != &tp {
                                    return traits::type_error_for_t(v, &tp).into();
                                }
                                v
                            }
                        }
                    },
                    types::PatternArgs { mut args } => {
                        let arg = args.next();
                        try_result!(args.unused_arguments());
                        match arg {
                            None => try_result!(constructor),
                            Some(v) => {
                                let deps = depends![v];
                                types::Unbound::new_no_doc(move |mut arg| {
                                    let v = v.clone();
                                    let tp = tp.clone();
                                    async move {
                                        drop(Context::eval(&mut arg).await);
                                        if arg.ergo_type().unwrap() != &tp {
                                            return traits::type_error_for_t(arg, &tp).into();
                                        }
                                        traits::bind(v, arg).await
                                    }.boxed()
                                }, deps).into()
                            }
                        }
                    },
                    mut v => {
                        drop(Context::eval(&mut v).await);
                        if let Some(t) = v.ergo_type() {
                            if t == &tp {
                                return v;
                            }
                        }
                        traits::bind_error(v).into()
                    }
                }
            }
            .boxed()
        },
        deps,
        doc,
    )
    .into()
}

fn match_unset() -> Value {
    make_match_fn(
        types::Unset::ergo_type(),
        Ok(types::Unset.into()),
        "Matches an Unset value.",
    )
}

fn match_unit() -> Value {
    make_match_fn(
        types::Unit::ergo_type(),
        Err("cannot compose; use script syntax".into()),
        "Matches a Unit value.",
    )
}

fn match_bool() -> Value {
    make_match_fn(
        types::Bool::ergo_type(),
        Err("cannot compose; use true and false indices".into()),
        "Matches a Bool value.",
    )
}

fn match_string() -> Value {
    make_match_fn(
        types::String::ergo_type(),
        Err("cannot compose; use script syntax".into()),
        "Matches a String value.",
    )
}

fn match_map() -> Value {
    make_match_fn(
        types::Map::ergo_type(),
        Err("cannot compose; use script syntax".into()),
        "Matches a Map value.",
    )
}

fn match_map_entry() -> Value {
    let construct: Value =
        types::Unbound::new_no_doc(
            |arg| {
                async move {
                    match_value!{ arg,
                        types::Args { mut args } => {
                            let key = try_result!(args.next().ok_or("no key"));
                            let value = try_result!(args.next().ok_or("no value"));
                            try_result!(args.unused_arguments());
                            types::MapEntry { key, value }.into()
                        },
                        types::PatternArgs { mut args } => {
                            let key = try_result!(args.next().ok_or("no key pattern"));
                            let value = try_result!(args.next().ok_or("no value pattern"));
                            try_result!(args.unused_arguments());
                            let deps = depends![key, value];
                            types::Unbound::new_no_doc(move |arg| {
                                let key = key.clone();
                                let value = value.clone();
                                async move {
                                    let entry = try_result!(Context::eval_as::<types::MapEntry>(arg).await).to_owned();
                                    try_result!(traits::bind_no_error(key, entry.key).await);
                                    try_result!(traits::bind_no_error(value, entry.value).await);
                                    types::Unit.into()
                                }.boxed()
                            }, deps).into()
                        },
                        v => traits::bind_error(v).into()
                    }
                }
                .boxed()
            },
            depends![nsid!(std::type::MapEntry::construct)],
        )
        .into();
    make_match_fn(
        types::MapEntry::ergo_type(),
        Ok(construct),
        "Matches a MapEntry value.

If called with no arguments, evaluates to a MapEntry constructor which accepts a key and value
argument in a call or pattern call.",
    )
}

fn match_array() -> Value {
    make_match_fn(
        types::Array::ergo_type(),
        Err("cannot compose; use script syntax".into()),
        "Matches an Array value.",
    )
}

fn match_iter() -> Value {
    make_match_fn(
        types::Iter::ergo_type(),
        Err("cannot compose; convert to/from other types".into()),
        "Matches an Iter value.",
    )
}

fn match_path() -> Value {
    make_match_fn(
        types::Path::ergo_type(),
        Err("cannot compose; convert to/from other types".into()),
        "Matches a Path value.",
    )
}

fn match_function() -> Value {
    make_match_fn(
        types::Unbound::ergo_type(),
        Err("cannot compose; use script syntax".into()),
        "Matches a Function value.",
    )
}

fn match_number() -> Value {
    let construct: Value = types::Unbound::new_no_doc(
            |arg| {
                async move {
                    match_value!{ arg,
                        types::Args { mut args } => {
                            let s = try_result!(args.next().ok_or("no argument"));
                            try_result!(args.unused_arguments());

                            let s = try_result!(Context::eval_as::<types::String>(s).await);
                            s.as_ref().0.as_str().parse::<types::Number>().map_err(|e| Source::get(&s).with(e)).into()
                        },
                        v => traits::bind_error(v).into()
                    }
                }
                .boxed()
            },
            depends![nsid!(std::type::Number::construct)],
    ).into();

    make_match_fn(
        types::Number::ergo_type(),
        Ok(construct),
        "Matches a Number value.

If called with no arguments, returns a number constructor which takes a numeric string argument.
The string may be an integer, decimal, or ratio of integers (e.g. `1/2`) with any number of digits.
The function can only be used in calls: you cannot destructure Numbers in pattern calls.",
    )
}

fn match_error() -> Value {
    let construct: Value = types::Unbound::new_no_doc(
        |arg| {
            async move {
                match_value! { arg,
                    types::Args { mut args } => {
                        let message = try_result!(args.next().ok_or("no message"));
                        let source = args.kw("source");
                        try_result!(args.unused_arguments());

                        let source = match source {
                            None => Source::get(&message),
                            Some(v) => Source::get(&v)
                        };

                        let message = try_result!(traits::to_string(message).await);
                        source.with(message).into_error().into()
                    },
                    v => traits::bind_error(v).into()
                }
            }
            .boxed()
        },
        depends![nsid!(std::type::Error::construct)],
    )
    .into();
    make_match_fn(
        types::Error::ergo_type(),
        Ok(construct),
        "Matches an Error value.

If called with no arguments, returns an error constructor which takes an error message and an
optional `source` keyed argument with a value from which source information will be used for the
error. The function can only be used in calls: you cannot destructure Errors in pattern calls.",
    )
}

#[cfg(test)]
mod test {
    ergo_script::tests! {
        fn any(t) {
            t.assert_eq("self:type:Any hello", "hello");
            t.assert_eq("self:type:Any :x = hello; :x", "hello");
            t.assert_success("!self:type:Any = hello");
        }

        fn unset(t) {
            t.assert_success("self:type:Unset {}:key");
            t.assert_fail("self:type:Unset str");
            t.assert_success("fn (self:type:Unset :x) -> () |> {}:key");
            t.assert_success("fn self:type:Unset -> () |> {}:key");
            t.assert_success("fn (self:type:Unset :x) -> () |> self:type:Unset:");
        }

        fn unit(t) {
            t.assert_eq("self:type:Unit ()", "()");
            t.assert_fail("self:type:Unit str");
            t.assert_eq("self:type:Unit :x = (); :x", "()");
            t.assert_fail("fn (self:type:Unit :x) -> () |> str");
            t.assert_success("fn self:type:Unit -> () |> ()");
            t.assert_fail("fn self:type:Unit -> () |> str");
        }

        fn bool(t) {
            t.assert_success("fn (self:type:Bool :b) -> () |> self:bool:true");
        }

        fn error(t) {
            t.assert_eq("bind (self:type:Error :e -> ()) (self:type:Error: doh)", "()");
            t.assert_eq("a=1; bind (self:type:Error :e -> ()) (self:type:Error: (source = :a) doh)", "()");
        }

        fn string(t) {
            t.assert_eq("self:type:String hi", "hi");
            t.assert_fail("self:type:String ()");
            t.assert_eq("self:type:String :x = hi; :x", "hi");
            t.assert_fail("fn (self:type:String :x) -> () |> ()");
            t.assert_success("fn self:type:String -> () |> hi");
            t.assert_fail("fn self:type:String -> () |> ()");
        }

        fn map_entry(t) {
            t.assert_script_success("self:type:MapEntry: k v");
            t.assert_eq("(self:type:MapEntry: k v):key","k");
            t.assert_eq("(self:type:MapEntry: k v):value","v");
        }

        fn function(t) {
            t.assert_success("self:type:Function (:a -> :a)");
            t.assert_fail("self:type:Function ()");
            t.assert_success("self:type:Function :x = :a -> :a; :x");
            t.assert_fail("fn (self:type:Function :x) -> () |> ()");
            t.assert_success("fn self:type:Function -> () |> :a -> :a");
            t.assert_fail("fn self:type:Function -> () |> ()");
        }

        fn number(t) {
            t.assert_success("self:type:Number: 1");
            t.assert_success("self:type:Number: -3.14");
            t.assert_success("self:type:Number: 1/2");
            t.assert_success("self:type:Number: -3/4");
            t.assert_success("self:type:Number: 0.5");
            t.assert_success("self:type:Number: -0.3");
            t.assert_eq("self:type:Number: 0.5", "self:type:Number: 1/2");
            t.assert_fail("self:type:Number: ()");
            t.assert_success("fn (self:type:Number :x) -> () |><| self:type:Number: 0");
        }

        // omit map/array/path/etc, they use the same macro so should be fundamentally the same

        fn new(t) {
            t.assert_content_eq("my_type = self:type:new \"scoped:my_type\" (:a -> !self:match :a [
                fn (self:type:String :a) (self:type:Unit :b) -> [:a,:b]
                pat :a :b -> [:a2,:b2] -> { bind :a :a2; bind :b :b2 }
            ])
            val = my_type: str ()
            fn !:my_type -> () |> :val
            fn (my_type _) -> () |> :val
            my_type: :x :y = :val
            [:x,:y]",
                "[str,()]"
            );
            t.assert_fail("my_type = self:type:new \"scoped:my_type\" (:a -> !self:match :a [
                fn (self:type:String :a) (self:type:Unit :b) -> [:a,:b]
                pat :a :b -> [:a2,:b2] -> { bind :a :a2; bind :b :b2 }
            ])
            fn !:my_type -> () |> ()");
        }

        fn new_bind(t) {
            t.assert_content_eq("my_type = self:type:new \"scoped:my_type\" (:a -> !self:match :a [
                fn (self:type:String :a) (self:type:Unit :b) -> [:a,:b]
                pat :a :b -> [:a2,:b2] -> { bind :a :a2; bind :b :b2 }
            ])
            val = my_type: str ()
            val:0",
                "str"
            );
            t.assert_content_eq("my_type_bind = [:a,:b] -> index string -> :a
            my_type = self:type:new (bind = :my_type_bind) \"scoped:my_type\" (:a -> !self:match :a [
                fn (self:type:String :a) (self:type:Unit :b) -> [:a,:b]
                pat :a :b -> [:a2,:b2] -> { bind :a :a2; bind :b :b2 }
            ])
            val = my_type: str ()
            val:string",
                "str"
            );
        }
    }
}
