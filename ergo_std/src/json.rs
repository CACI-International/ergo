//! JSON functions.

use ergo_runtime::{
    error::DiagnosticInfo, metadata::Source, traits, types, value::match_value, Context, Value,
};
use futures::future::{BoxFuture, FutureExt};
use json::{self, JsonValue};

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
    let json_val = json::parse(json.as_ref().0.as_str())
        .add_primary_label(Source::get(&json).with("while parsing JSON from this value"))?;

    fn json_to_val(src: &ergo_runtime::Source<()>, json: JsonValue) -> Value {
        match json {
            JsonValue::Null => types::Unit.into(),
            JsonValue::Short(s) => types::String::from(s.as_str()).into(),
            JsonValue::String(s) => types::String::from(s).into(),
            JsonValue::Number(n) => match types::Number::from_f64(n.into()) {
                // FIXME this case should technically be a panic, as a json number is guaranteed to
                // not be +/-inf or NaN, which are the cases under which from_f64 would fail.
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
                            crate::make_string_src(src.clone().with(k)),
                            Source::imbue(src.clone().with(json_to_val(src, v.clone()))),
                        )
                    })
                    .collect(),
            )
            .into(),
            JsonValue::Array(a) => types::Array(
                a.into_iter()
                    .map(|v| Source::imbue(src.clone().with(json_to_val(src, v))))
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
///
/// Keyed Arguments:
/// `:pretty` - if present, the output will be pretty-printed.
async fn stringify(value: _, (pretty): [_]) -> Value {
    fn val_to_json(mut val: Value) -> BoxFuture<'static, ergo_runtime::Result<JsonValue>> {
        async move {
            Context::eval(&mut val).await?;
            let val_source = Source::get(&val);
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
                        let k = Context::eval_as::<types::String>(k.into()).await;
                        let v = val_to_json(v).await;
                        entries.push(match (k,v) {
                            (Err(ke), Err(ve)) => Err(ergo_runtime::Error::aggregate(vec![ke,ve])),
                            (Err(e), _) | (_, Err(e)) => Err(e),
                            (Ok(k), Ok(v)) => Ok((k.to_owned().0,v))
                        });
                    }
                    Ok(JsonValue::Object(entries.into_iter().collect::<Result<Vec<_>, _>>()?.into_iter().collect()))
                }
                types::Array(a) => {
                    let mut entries = Vec::new();
                    for v in a {
                        entries.push(val_to_json(v).await);
                    }
                    Ok(JsonValue::Array(entries.into_iter().collect::<Result<Vec<_>, _>>()?))
                }
                o => Err(traits::type_error(o, "json-compatible type").into())
            }
        }
        .boxed()
    }

    let pretty = pretty.is_some();

    let val = val_to_json(value).await?;
    types::String::from(if pretty {
        json::stringify_pretty(val, 4)
    } else {
        json::stringify(val)
    })
    .into()
}

#[cfg(test)]
mod test {
    ergo_script::tests! {
        fn parse(t) {
            t.assert_eq(r#"self:json:parse '
            ' {"a": null, "b": 1, "c": true, "d": ["str"]}"#, "{
                    a = (), b = self:Number:from 1, c = self:Bool:true, d = [str]
                }");
        }

        fn stringify(t) {
            t.assert_eq("self:json:parse <| self:json:stringify {
                    a = (), b = self:Number:from 1, c = self:Bool:true, d = [str]
                }",
                "{
                    a = (), b = self:Number:from 1, c = self:Bool:true, d = [str]
                }");
        }

        fn stringify_pretty(t) {
            t.assert_eq("self:json:parse <| self:json:stringify ^pretty {
                    a = (), b = self:Number:from 1, c = self:Bool:true, d = [str]
                }",
                "{ a = (), b = self:Number:from 1, c = self:Bool:true, d = [str] }"
            );
        }
    }
}
