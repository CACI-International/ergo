//! JSON functions.

use ergo_runtime::{traits, try_result, types, value::match_value, Context, Source, Value};
use futures::future::{BoxFuture, FutureExt};
use json::{parse as json_parse, stringify as json_stringify, JsonValue};

pub fn module() -> Value {
    crate::make_string_map! {
        "parse" = parse(),
        "stringify" = stringify()
    }
}

#[types::ergo_fn]
/// Parse a JSON string into native ergo types.
///
/// Arguments: `String :json`
async fn parse(json: types::String) -> Value {
    let (json_source, json) = json.take();
    let json_val = match json_parse(json.as_ref().0.as_str()) {
        Err(e) => return json_source.with(e).into_error().into(),
        Ok(val) => val,
    };

    fn json_to_val(src: &Source<()>, json: JsonValue) -> Value {
        match json {
            JsonValue::Null => types::Unit.into(),
            JsonValue::Short(s) => types::String::from(s.as_str()).into(),
            JsonValue::String(s) => types::String::from(s).into(),
            JsonValue::Number(n) => match types::Number::from_f64(n.into()) {
                None => src
                    .clone()
                    .with(format!("invalid number {}", n))
                    .into_error()
                    .into(),
                Some(n) => n.into(),
            },
            JsonValue::Boolean(b) => types::Bool(b).into(),
            JsonValue::Object(o) => types::Map(
                o.iter()
                    .map(|(k, v)| {
                        (
                            src.clone().with(types::String::from(k).into()),
                            src.clone().with(json_to_val(src, v.clone())),
                        )
                    })
                    .collect(),
            )
            .into(),
            JsonValue::Array(a) => types::Array(
                a.into_iter()
                    .map(|v| src.clone().with(json_to_val(src, v)))
                    .collect(),
            )
            .into(),
        }
    }

    json_to_val(&ARGS_SOURCE, json_val)
}

#[types::ergo_fn]
/// Convert the given native ergo value to a JSON string.
///
/// Arguments: `:value`
async fn stringify(value: _) -> Value {
    fn val_to_json<'a>(
        ctx: &'a Context,
        val: Source<Value>,
    ) -> BoxFuture<'a, ergo_runtime::Result<JsonValue>> {
        async move {
            let (val_source, mut val) = val.take();
            ctx.eval(&mut val).await?;
            match_value! { val,
                types::Unit => Ok(JsonValue::Null),
                types::String(s) => Ok(JsonValue::String(s.into())),
                n@types::Number {..} => match n.to_f64() {
                    None => Err(val_source.with("could not convert number to json value").into_error()),
                    Some(f) => Ok(JsonValue::Number(f.into())),
                }
                types::Bool(b) => Ok(JsonValue::Boolean(b)),
                types::Map(m) => {
                    let mut entries = Vec::new();
                    for (k,v) in m {
                        let k = ctx.eval_as::<types::String>(k).await;
                        let v = val_to_json(ctx, v).await;
                        entries.push(match (k,v) {
                            (Err(ke), Err(ve)) => Err(ergo_runtime::Error::aggregate(vec![ke,ve])),
                            (Err(e), _) | (_, Err(e)) => Err(e),
                            (Ok(k), Ok(v)) => Ok((k.unwrap().to_owned().0,v))
                        });
                    }
                    Ok(JsonValue::Object(entries.into_iter().collect::<Result<Vec<_>, _>>()?.into_iter().collect()))
                }
                types::Array(a) => {
                    let mut entries = Vec::new();
                    for v in a {
                        entries.push(val_to_json(ctx, v).await);
                    }
                    Ok(JsonValue::Array(entries.into_iter().collect::<Result<Vec<_>, _>>()?))
                }
                o => Err(traits::type_error(ctx, val_source.with(o), "json-compatible type"))
            }
        }
        .boxed()
    }

    let val = try_result!(val_to_json(CONTEXT, value).await);
    types::String::from(json_stringify(val)).into()
}

#[cfg(test)]
mod test {
    ergo_script::test! {
        fn parse(t) {
            t.assert_content_eq(r#"self:json:parse '{"a": null, "b": 1, "c": true, "d": ["str"]}'"#, "{
                    a = (), b = self:type:Number: 1, c = self:bool:true, d = [str]
                }");
        }
    }

    ergo_script::test! {
        fn stringify(t) {
            t.assert_content_eq("self:json:stringify {
                    a = (), b = self:type:Number: 1, c = self:bool:true, d = [str]
                }",
                // The actual string is subject to value identity ordering of keys.
                r#"'{"a":null,"d":["str"],"c":true,"b":1}'"#
            );
        }
    }
}
