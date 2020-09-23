//! String manipulation functions.

use ergo_runtime::{ergo_function, source_value_as, traits, types};
use grease::{bst::BstMap, make_value, value::Value};
use std::fmt::Write;

pub fn module() -> Value {
    let mut map = BstMap::default();
    map.insert("format".into(), format_fn());
    map.insert("from".into(), from_fn());
    map.insert("split".into(), split_fn());
    map.insert("trim".into(), trim_fn());
    types::Map(map).into()
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

    pub fn iter(&self) -> std::slice::Iter<StringFragment> {
        self.fragments.iter()
    }
}

impl From<&'_ StringFragment> for grease::value::Dependency {
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

fn format_fn() -> Value {
    ergo_function!(std::string::format, |ctx| {
        let format_str = ctx.args.next().ok_or("no format string provided")?;
        let source = format_str.source();
        let format_str = {
            let s = source_value_as!(format_str, types::String, ctx)?;
            s.await.unwrap()?
        };

        let pos_args: Vec<_> = ctx.args.by_ref().collect();
        let kw_args = std::mem::take(&mut ctx.args.non_positional);

        let mut fragments = Fragments::default();

        let mut arg: Option<String> = None;

        let mut pos_arg_next = 0;

        let format_str = format_str.owned().into_string();
        let mut iter = format_str.chars().peekable();
        while let Some(c) = iter.next() {
            if c == '{' {
                if arg.is_some() {
                    return Err(source.with("invalid format string: '{' found within a format argument").into_grease_error());
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
                            return Err(source.with("invalid format string: '}' found without preceding '{'").into_grease_error());
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
                            (kw_args.get(v.as_str()),v)
                        };
                        match val {
                            None => {
                                return Err(format!("format string argument '{}' not found", disp).into());
                            },
                            Some(v) => {
                                match traits::try_display(&ctx.traits, &v) {
                                    Ok(_) => {
                                        fragments.push_value(v.as_ref().unwrap().clone());
                                        arg = None;
                                    },
                                    Err(e) => {
                                        return Err(v.source().with(e).into_grease_error());
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
            return Err("'{' without matching '}'".into())
        } else {
            let traits = ctx.traits.clone();
            let task = ctx.task.clone();
            let deps = grease::depends![^@fragments];
            make_value!([^deps] {
                let values: Vec<_> = fragments.into_iter().filter_map(|v| match v { StringFragment::Value(v) => Some(v), _ => None })
                    .map(|v| traits::force_value_nested(&traits, v.clone()))
                    .collect();
                task.join_all(values).await?;

                let mut result = types::String::new();
                for f in fragments.into_iter() {
                    match f {
                        StringFragment::Literal(s) => result.push_str(&s),
                        StringFragment::Value(v) => {
                            write!(result, "{}", traits::display(&traits, &v))?;
                        }
                    }
                }
                Ok(result)
            }).into()
        }
    }).into()
}

fn from_fn() -> Value {
    ergo_function!(independent std::string::from, |ctx| {
        let value = ctx.args.next().ok_or("value not provided")?;

        ctx.unused_arguments()?;

        ctx.traits
            .get::<traits::IntoTyped<types::String>>(&value)
            .ok_or(
                value
                    .source()
                    .with("cannot convert value into string")
                    .into_grease_error(),
            )?
            .into_typed(value.unwrap())
            .into()
    })
    .into()
}

fn split_fn() -> Value {
    ergo_function!(std::string::split, |ctx| {
        let pat = ctx.args.next().ok_or("split pattern not provided")?;
        let s = ctx.args.next().ok_or("string not provided")?;

        ctx.unused_arguments()?;

        let pat = source_value_as!(pat, types::String, ctx)?.unwrap();
        let s = source_value_as!(s, types::String, ctx)?.unwrap();

        let task = ctx.task.clone();
        make_value!([s, pat] {
            let (s, pat) = task.join(s,pat).await?;
            let v = s.as_ref().as_str().split(pat.as_ref().as_str()).map(|s| types::String::from(s).into()).collect();
            Ok(types::Array(v))
        })
        .into()
    })
    .into()
}

fn trim_fn() -> Value {
    ergo_function!(std::string::trim, |ctx| {
        let s = ctx.args.next().ok_or("string not provided")?;

        ctx.unused_arguments()?;

        let s = source_value_as!(s, types::String, ctx)?.unwrap();

        make_value!([s] {
            let s = s.await?;
            Ok(types::String::from(s.as_ref().as_str().trim()))
        })
        .into()
    })
    .into()
}
