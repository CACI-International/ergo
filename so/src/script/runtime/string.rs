//! String manipulation functions.

use super::builtin_function_prelude::*;
use std::collections::BTreeMap;
use std::fmt::Write;

pub fn builtin() -> Value {
    let mut map = BTreeMap::new();
    map.insert("format".to_owned(), format_fn());
    ScriptMap(map).into()
}

script_fn!(format_fn, ctx => {
    let format_str = ctx.args.next().ok_or("no format string provided")?;
    let source = format_str.source();
    let format_str = script_value_as!(ctx, format_str, ScriptString, "format string must be a string");

    let pos_args: Vec<_> = ctx.args.by_ref().collect();
    let kw_args = std::mem::take(&mut ctx.args.non_positional);

    let mut result = ScriptString::new();

    let mut arg: Option<String> = None;

    let mut pos_arg_next = 0;

    let mut iter = format_str.chars().peekable();
    while let Some(c) = iter.next() {
        if c == '{' {
            if arg.is_some() {
                ctx.error(source.with("invalid format string: '{' found within a format argument"));
                return Ok(Eval::Error);
            }
            else if let Some('{') = iter.peek() {
                iter.next().unwrap();
                result.push('{');
            } else {
                arg = Some(Default::default());
            }
        } else if c == '}' {
            match arg {
                None => {
                    if let Some('}') = iter.peek() {
                        iter.next().unwrap();
                        result.push('}');
                    } else {
                        ctx.error(source.with("invalid format string: '}' found without preceding '{'"));
                        return Ok(Eval::Error);
                    }
                },
                Some(v) => {
                    let (val,disp) = if v.is_empty() {
                        let v = (pos_args.get(pos_arg_next), pos_arg_next.to_string());
                        pos_arg_next += 1;
                        v
                    } else if let Ok(i) = v.parse::<usize>() {
                        (pos_args.get(i),v)
                    } else {
                        (kw_args.get(&v),v)
                    };
                    match val {
                        None => {
                            return Err(format!("format string argument '{}' not found", disp).into());
                        },
                        Some(v) => {
                            match grease::try_display(&ctx.traits, &v) {
                                Ok(d) => {
                                    eval_error!(ctx, v.clone().map(|v| futures::executor::block_on(
                                            crate::script::traits::nested::force_value_nested(&ctx.traits, v))
                                        ).transpose_err());
                                    write!(result, "{}", d).map_err(|_| "failed to write format string")?;
                                    arg = None;
                                },
                                Err(e) => {
                                    ctx.error(v.source().with(e));
                                    return Ok(Eval::Error);
                                }
                            }
                        }
                    }
                }
            }
        } else {
            match &mut arg {
                None => result.push(c),
                Some(ref mut s) => s.push(c),
            }
        }
    }

    if arg.is_some() {
        Err("'{' without matching '}'".into())
    } else {
        Ok(Eval::Value(result.into()))
    }
});
