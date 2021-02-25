//! String manipulation functions.

use ergo_runtime::{context_ext::AsContext, ergo_function, types, ContextExt};
use grease::{make_value, value::Value};
use std::fmt::Write;

pub fn module() -> Value {
    crate::grease_string_map! {
        "format" = format_fn(),
        "from" = from_fn(),
        "join" = join_fn(),
        "split" = split_fn(),
        "trim" = trim_fn()
    }
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
    ergo_function!(std::string::format,
    r#"Create a string based on a format specification.

Arguments: <format string: String> [arguments...]

Keyword Arguments: any

The format string may contain curly brackets to indicate arguments to insert in the string. The curly brackets may be
empty (`{}`) or may contain a numeric index to indicate which positional argument to use, or a string index to indicate
which non-positional argument to use. Empty brackets will use the next positional argument unused by other empty
brackets. Note that since the curly brackets are syntactically-significant, a format string containing them will need
to be quoted. To insert a literal open curly bracket, use `{{`, and likewise `}}` for a literal close curly bracket.

Examples:
`format "hello {}" world` => `"hello world"`
`format "hello {name}" ^{name = billy}` => `"hello billy"`
`format "{}, {a}, {}, {0}, and {{{}}}" ^{a = alice} bob cal daisy` => `"bob, alice, cal, bob, and {daisy}"`"#,
    |ctx, args| {
        let format_str = args.next().ok_or("no format string provided")?;
        let source = format_str.source();
        let format_str = {
            let s = ctx.source_value_as::<types::String>(format_str);
            s.await?.await.unwrap()?
        };

        let pos_args: Vec<_> = args.by_ref().collect();
        let kw_args = std::mem::take(&mut args.non_positional);

        args.unused_arguments()?;

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
                            (kw_args.get(&crate::grease_string(v.as_str())),v)
                        };
                        match val {
                            None => {
                                return Err(format!("format string argument '{}' not found", disp).into());
                            },
                            Some(v) => {
                                fragments.push_value(v.as_ref().unwrap().clone());
                                arg = None;
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
            let ctx = ctx.as_context().clone();
            let deps = grease::depends![^@fragments];
            make_value!([^deps] {
                let values: Vec<_> = fragments.into_iter().filter_map(|v| match v { StringFragment::Value(v) => Some(v), _ => None })
                    .map(|v| ctx.force_value_nested(v.clone()))
                    .collect();
                ctx.task.join_all(values).await?;

                let mut result = types::String::new();
                for f in fragments.into_iter() {
                    match f {
                        StringFragment::Literal(s) => result.push_str(&s),
                        StringFragment::Value(v) => {
                            write!(result, "{}", ctx.display(v.clone()).await?)?;
                        }
                    }
                }
                Ok(result)
            }).into()
        }
    }).into()
}

fn from_fn() -> Value {
    ergo_function!(independent std::string::from,
    r"Convert a value into a string.

Arguments: <value: Into<String>>

Returns the result of converting the value into a string.",
    |ctx, args| {
        let value = args.next().ok_or("value not provided")?;

        args.unused_arguments()?;

        let v = ctx.into_sourced::<types::String>(value);
        v.await?.unwrap().into()
    })
    .into()
}

fn split_fn() -> Value {
    ergo_function!(std::string::split,
    r"Split a string on a substring.

Arguments: <pattern: String> <str: String>

Returns an Array of Strings representing the segments of `str` separated by `pattern`.",
    |ctx, args| {
        let pat = args.next().ok_or("split pattern not provided")?;
        let s = args.next().ok_or("string not provided")?;

        args.unused_arguments()?;

        let pat = ctx.source_value_as::<types::String>(pat);
        let pat = pat.await?.unwrap();
        let s = ctx.source_value_as::<types::String>(s);
        let s = s.await?.unwrap();

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

fn join_fn() -> Value {
    ergo_function!(std::string::split,
    r"Join an array of strings.

Arguments: <separator: String> <iter: Iter>

Returns a String representing the strings in `iter` separated by `separator`.",
    |ctx, args| {
        let sep = args.next().ok_or("join separator not provided")?;
        let iter = args.next().ok_or("iter not provided")?;

        args.unused_arguments()?;

        let sep = ctx.source_value_as::<types::String>(sep).await?.unwrap();
        let (iter_source, iter) = ctx.into_sourced::<types::Iter>(iter).await?.take();

        let ctx = ctx.empty();
        make_value!([sep, iter] {
            let (sep, iter) = ctx.task.join(sep,iter).await?;
            let iter = iter.owned();
            let vals: Vec<_> = iter.try_collect().await?;
            let strs: Vec<_> = vals.into_iter().map(|v| ctx.source_value_as::<types::String>(iter_source.clone().with(v))).collect();
            // Evaluate source_value_as
            let strs = ctx.task.join_all(strs).await?.into_iter().map(|sv| sv.unwrap());
            // Evaluate resulting TypedValue<types::String>
            let strs = ctx.task.join_all(strs).await?;
            let strs = strs.iter().map(|v| v.as_str()).collect::<Vec<_>>();
            Ok(types::String::from(strs.join(sep.0.as_str())))
        })
        .into()
    })
    .into()
}

fn trim_fn() -> Value {
    ergo_function!(
        std::string::trim,
        r"Trim whitespace from the beginning and end of a string.

Arguments: <String>",
        |ctx, args| {
            let s = args.next().ok_or("string not provided")?;

            args.unused_arguments()?;

            let s = ctx.source_value_as::<types::String>(s);
            let s = s.await?.unwrap();

            make_value!([s] {
                let s = s.await?;
                Ok(types::String::from(s.as_ref().as_str().trim()))
            })
            .into()
        }
    )
    .into()
}

#[cfg(test)]
mod test {
    use ergo_runtime::types::String;

    ergo_script::test! {
        fn format(t) {
            t.assert_value_eq("self:string:format \"hello {}\" world", &String::from("hello world"));
            t.assert_value_eq("self:string:format \"{1}{}{2}{0}\" a b c d", &String::from("baca"));
            t.assert_value_eq(
                "self:string:format \"{my_named_arg} {}\" (:my_named_arg = howdy) hi",
                &String::from("howdy hi"),
            );
            t.assert_value_eq("self:string:format \"{{{{}}\"", &String::from("{{}"));
            t.assert_script_fail("self:string:format \"{\"");
            t.assert_script_fail("self:string:format \"}\"");
            t.assert_script_fail("self:string:format \"{}\"");
            t.assert_script_fail("self:string:format \"{named}\" ^{:not-named=1}");
            t.assert_script_fail("self:string:format \"{{{}}\" a");
        }
    }

    ergo_script::test! {
        fn split(t) {
            t.assert_content_eq(r#"self:string:split l "hello world""#, r#"[he,"","o wor",d]"#);
            t.assert_content_eq(r#"self:string:split the "the fox jumps over the fence""#, r#"[""," fox jumps over "," fence"]"#);
            t.assert_content_eq(r#"self:string:split " " "the fox jumps over the fence""#, "[the,fox,jumps,over,the,fence]");
            t.assert_content_eq("self:string:split t tttt", r#"["","","","",""]"#);
        }
    }

    ergo_script::test! {
        fn join(t) {
            t.assert_content_eq(r#"self:string:join l [he,"","o wor",d]"#, r#""hello world""#);
            t.assert_content_eq(r#"self:string:join " " [the,fox,jumps,over,the,fence]"#, r#""the fox jumps over the fence""#);
            t.assert_content_eq(r#"self:string:join v ["","","","",""]"#, "vvvv");
        }
    }

    ergo_script::test! {
        fn trim(t) {
            t.assert_content_eq(r#"self:string:trim "
            something

            ""#, "something");
        }
    }
}
