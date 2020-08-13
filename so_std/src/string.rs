//! String manipulation functions.

use super::builtin_function_prelude::*;
use grease::make_value;
use std::collections::BTreeMap;
use std::fmt::Write;

pub fn builtin() -> Value {
    let mut map = BTreeMap::new();
    map.insert("format".to_owned(), format_fn());
    ScriptMap(map).into()
}

#[derive(Debug)]
enum StringFragment {
    Literal(String),
    Value(Value),
}

#[derive(Debug, Default)]
struct Fragments {
    fragments: Vec<StringFragment>,
}

impl Fragments {
    pub fn push(&mut self, c: char) {
        match self.fragments.last_mut() {
            Some(StringFragment::Literal(ref mut s)) => s.push(c),
            _ => self.fragments.push(StringFragment::Literal(c.to_string())),
        }
    }

    pub fn push_value(&mut self, v: Value) {
        self.fragments.push(StringFragment::Value(v))
    }
}

impl From<&'_ StringFragment> for grease::Dependency {
    fn from(sf: &StringFragment) -> Self {
        match sf {
            StringFragment::Literal(s) => s.into(),
            StringFragment::Value(s) => s.into(),
        }
    }
}

impl<'a> IntoIterator for &'a Fragments {
    type Item = &'a StringFragment;
    type IntoIter = std::slice::Iter<'a, StringFragment>;

    fn into_iter(self) -> Self::IntoIter {
        (&self.fragments).into_iter()
    }
}

script_fn!(format_fn, ctx => {
    let format_str = ctx.args.next().ok_or("no format string provided")?;
    let source = format_str.source();
    let format_str = script_value_as!(ctx, format_str, ScriptString, "format string must be a string");

    let pos_args: Vec<_> = ctx.args.by_ref().collect();
    let kw_args = std::mem::take(&mut ctx.args.non_positional);

    let mut fragments = Fragments::default();

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
                fragments.push('{');
            } else {
                arg = Some(Default::default());
            }
        } else if c == '}' {
            match arg {
                None => {
                    if let Some('}') = iter.peek() {
                        iter.next().unwrap();
                        fragments.push('}');
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
                                Ok(_) => {
                                    fragments.push_value(v.as_ref().unwrap().clone());
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
                None => fragments.push(c),
                Some(ref mut s) => s.push(c),
            }
        }
    }

    if arg.is_some() {
        Err("'{' without matching '}'".into())
    } else {
        let traits = ctx.traits.clone();
        let task = ctx.task.clone();
        Ok(Eval::Value(make_value!([^fragments] {
            let values: Vec<_> = fragments.into_iter().filter_map(|v| match v { StringFragment::Value(v) => Some(v), _ => None })
                .map(|v| crate::script::traits::nested::force_value_nested(&traits, v.clone()))
                .collect();
            task.join_all(values).await?;

            let mut result = ScriptString::new();
            for f in fragments.into_iter() {
                match f {
                    StringFragment::Literal(s) => result.push_str(&s),
                    StringFragment::Value(v) => {
                        write!(result, "{}", grease::display(&traits, &v))?;
                    }
                }
            }
            Ok(result)
        }).into()))
    }
});
