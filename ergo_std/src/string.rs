//! String manipulation functions.

use ergo_runtime::{source_value_as, traits, types};
use futures::future::FutureExt;
use grease::{bst::BstMap, make_value, value::Value};
use std::fmt::Write;

pub fn module() -> Value {
    let mut map = BstMap::default();
    map.insert("format".into(), format_fn());
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
    types::Function::new(|ctx| async move {
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
        Err("'{' without matching '}'".into())
    } else {
        let traits = ctx.traits.clone();
        let task = ctx.task.clone();
        let deps = grease::depends![^@fragments];
        Ok(ctx.call_site.clone().with(make_value!([^deps] {
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
        }).into()))
    }
}.boxed()).into()
}
