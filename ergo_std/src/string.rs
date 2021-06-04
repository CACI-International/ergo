//! String manipulation functions.

use ergo_runtime::{traits, try_result, types, Value};

pub fn module() -> Value {
    crate::make_string_map! {
        "format" = format(),
        "from" = from(),
        "join" = join(),
        "split" = split(),
        "trim" = trim()
    }
}

#[types::ergo_fn]
/// Create a string based on a format specification.
///
/// Arguments: `(String :format-string) ^:arguments`
///
/// The format string may contain curly brackets to indicate arguments to insert in the string. The
/// curly brackets may be empty (`{}`) or may contain a numeric index to indicate which positional
/// argument to use, or a string index to indicate which keyed argument to use. Empty brackets will
/// use the next positional argument unused by other empty brackets. Note that since the curly
/// brackets are syntactically-significant, a format string containing them will need to be quoted.
/// To insert a literal open curly bracket, use `{{`, and likewise `}}` for a literal close curly
/// bracket.
///
/// ### Examples
/// * `format "hello {}" world` => `"hello world"`
/// * `format "hello {name}" (name = billy)` => `"hello billy"`
/// * `format "{}, {a}, {}, {0}, and {{{}}}" (a = alice) bob cal daisy` => `"bob, alice, cal, bob, and {daisy}"`
async fn format(format_string: types::String, ...) -> Value {
    let pos_args: Vec<_> = REST.by_ref().collect();
    let keyed_args = std::mem::take(&mut REST.keyed);

    try_result!(REST.unused_arguments());

    let mut result = String::new();
    let mut formatter = traits::Formatter::new(&mut result);

    let (source, fmt) = format_string.take();

    let mut arg: Option<String> = None;
    let mut pos_arg_next = 0;
    let fmt = fmt.to_owned().into_string();
    let mut iter = fmt.chars().peekable();
    use std::fmt::Write;
    while let Some(c) = iter.next() {
        if c == '{' {
            if arg.is_some() {
                return source
                    .with("invalid format string: '{' found within a format argument")
                    .into_error()
                    .into();
            } else if let Some('{') = iter.peek() {
                iter.next().unwrap();
                try_result!(formatter.write_char('{'));
            } else {
                arg = Some(Default::default());
            }
        } else if c == '}' {
            match arg {
                None => {
                    if let Some('}') = iter.peek() {
                        iter.next().unwrap();
                        try_result!(formatter.write_char('}'));
                    } else {
                        return source
                            .with("invalid format string: '}' found without preceding '{'")
                            .into_error()
                            .into();
                    }
                }
                Some(v) => {
                    let (val, disp) = if v.is_empty() {
                        let v = (pos_args.get(pos_arg_next), pos_arg_next.to_string());
                        pos_arg_next += 1;
                        v
                    } else if let Ok(i) = v.parse::<usize>() {
                        (pos_args.get(i), v)
                    } else {
                        (keyed_args.get(&crate::make_string(v.as_str())), v)
                    };
                    match val {
                        None => {
                            return source
                                .with(format!("format string argument '{}' not found", disp))
                                .into_error()
                                .into();
                        }
                        Some(v) => {
                            try_result!(
                                traits::display(CONTEXT, v.value().clone(), &mut formatter).await
                            );
                            arg = None;
                        }
                    }
                }
            }
        } else {
            match &mut arg {
                None => try_result!(formatter.write_char(c)),
                Some(ref mut s) => s.push(c),
            }
        }
    }

    if arg.is_some() {
        return source.with("'{' without matching '}'").into_error().into();
    }

    drop(formatter);

    types::String::from(result).into()
}

#[types::ergo_fn]
/// Convert a value into a string.
///
/// Arguments: `:value`
async fn from(value: _) -> Value {
    ergo_runtime::try_result!(traits::into_sourced::<types::String>(CONTEXT, value).await)
        .unwrap()
        .into()
}

#[types::ergo_fn]
/// Split a string on a substring.
///
/// Arguments: `(String :pattern) (String :str)`
///
/// Returns an `Array` of `String` representing the segments of `str` separated by `pattern`.
async fn split(pattern: types::String, s: types::String) -> Value {
    let v = s
        .value()
        .as_ref()
        .as_str()
        .split(pattern.value().as_ref().as_str())
        .map(|s| ARGS_SOURCE.clone().with(types::String::from(s).into()))
        .collect();
    types::Array(v).into()
}

#[types::ergo_fn]
/// Join an array of strings.
///
/// Arguments: `(String :separator) (Into<Iter> :iter)`
///
/// Returns a `String` representing the strings in `iter` separated by `separator`.
async fn join(separator: types::String, iter: _) -> Value {
    let iter = try_result!(traits::into_sourced::<types::Iter>(CONTEXT, iter).await).unwrap();

    let vals: Vec<_> = iter.to_owned().collect().await;
    let strs = try_result!(
        CONTEXT
            .task
            .join_all(
                vals.into_iter()
                    .map(|v| CONTEXT.eval_as::<types::String>(v))
            )
            .await
    );
    let strs = strs
        .iter()
        .map(|v| v.value().as_ref().as_str())
        .collect::<Vec<_>>();
    types::String::from(strs.join(separator.value().as_ref().as_str())).into()
}

#[types::ergo_fn]
/// Trim whitespace from the beginning and end of a string.
///
/// Arguments: `(String :str)`
///
/// Returns the trimmed string.
async fn trim(s: types::String) -> Value {
    types::String::from(s.value().as_ref().as_str().trim()).into()
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
            t.assert_fail("self:string:format \"{\"");
            t.assert_fail("self:string:format \"}\"");
            t.assert_fail("self:string:format \"{}\"");
            t.assert_fail("self:string:format \"{named}\" ^{:not-named=1}");
            t.assert_fail("self:string:format \"{{{}}\" a");
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
