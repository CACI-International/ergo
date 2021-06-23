//! String manipulation functions.

use ergo_runtime::{
    dependency::{AsDependency, Dependency},
    depends, nsid, traits, try_result, types,
    value::match_value,
    Source, Value,
};
use futures::future::FutureExt;

pub fn module() -> Value {
    crate::make_string_map! {
        "chars" = chars(),
        "format" = format(),
        "from" = from(),
        "join" = join(),
        "split" = split(),
        "trim" = trim()
    }
}

#[types::ergo_fn]
/// Create an iterator over the characters of a string.
///
/// Arguments: `(String :s)`
///
/// Returns an `Iter` where each item will be a `String` containing a single character.
async fn chars(s: types::String) -> Value {
    let v = s
        .unwrap()
        .to_owned()
        .chars()
        .map(|c| {
            ARGS_SOURCE
                .clone()
                .with(types::String::from(String::from(c)).into())
        })
        .collect::<Vec<_>>();

    types::Iter::new(v.into_iter(), CALL_DEPENDS).into()
}

fn format() -> Value {
    #[derive(Debug)]
    enum FormatPart {
        String(String),
        Positional(usize),
        Keyed(String),
    }

    #[derive(Debug, Default)]
    struct FormatParser {
        parts: Vec<FormatPart>,
        arg: Option<String>,
    }

    impl FormatParser {
        pub fn push_char(&mut self, c: char) {
            if let Some(s) = &mut self.arg {
                s.push(c);
            } else if let Some(FormatPart::String(s)) = self.parts.last_mut() {
                s.push(c);
            } else {
                self.parts.push(FormatPart::String(c.into()));
            }
        }

        pub fn start_arg(&mut self) {
            self.arg = Some(Default::default());
        }

        pub fn end_arg(&mut self) -> Option<String> {
            self.arg.take()
        }

        pub fn has_arg(&self) -> bool {
            self.arg.is_some()
        }

        pub fn positional_arg(&mut self, i: usize) {
            self.parts.push(FormatPart::Positional(i));
        }

        pub fn keyed_arg(&mut self, s: String) {
            self.parts.push(FormatPart::Keyed(s));
        }

        pub fn into_inner(self) -> Result<Vec<FormatPart>, &'static str> {
            if self.arg.is_some() {
                Err("'{' without matching '}'")
            } else {
                Ok(self.parts)
            }
        }
    }

    fn format_parts(fmt: &str) -> Result<Vec<FormatPart>, &'static str> {
        let mut pos_arg_next = 0;
        let mut iter = fmt.chars().peekable();

        let mut parser = FormatParser::default();

        while let Some(c) = iter.next() {
            if c == '{' {
                if parser.has_arg() {
                    return Err("invalid format string: '{' found within a format argument");
                } else if let Some('{') = iter.peek() {
                    iter.next().unwrap();
                    parser.push_char('{');
                } else {
                    parser.start_arg();
                }
            } else if c == '}' {
                match parser.end_arg() {
                    None => {
                        if let Some('}') = iter.peek() {
                            iter.next().unwrap();
                            parser.push_char('}');
                        } else {
                            return Err("invalid format string: '}' found without preceding '{'");
                        }
                    }
                    Some(v) => {
                        if v.is_empty() {
                            parser.positional_arg(pos_arg_next);
                            pos_arg_next += 1;
                        } else if let Ok(i) = v.parse::<usize>() {
                            parser.positional_arg(i);
                        } else {
                            parser.keyed_arg(v);
                        };
                    }
                }
            } else {
                parser.push_char(c);
            }
        }

        parser.into_inner()
    }

    types::Unbound::new(|ctx, arg| {
        async move {
            let (arg_source, arg) = arg.take();
            match_value!{ arg,
                types::Args { mut args } => {
                    let format_string = try_result!(args.next().ok_or("missing format string"));
                    let format_string = try_result!(ctx.eval_as::<types::String>(format_string).await);

                    let pos_args: Vec<_> = args.by_ref().collect();
                    let keyed_args = std::mem::take(&mut args.keyed);

                    try_result!(args.unused_arguments());

                    let mut result = String::new();
                    let mut formatter = traits::Formatter::new(&mut result);

                    let (source, parts) = try_result!(format_string.map(|s| format_parts(s.as_ref().as_str()))
                                                                    .transpose().map_err(|e| e.into_error())).take();
                    for part in parts {
                        match part {
                            FormatPart::String(s) => try_result!(formatter.write_str(&s)),
                            other => {
                                let (val, disp) = match other {
                                    FormatPart::Positional(p) => (pos_args.get(p), p.to_string()),
                                    FormatPart::Keyed(k) => (keyed_args.get(&crate::make_string(k.as_str())), k),
                                    _ => panic!("invalid format part")
                                };
                                match val {
                                    None => return source.with(format!("format string argument '{}' not found", disp)).into_error().into(),
                                    Some(v) => try_result!(traits::display(ctx, v.value().clone(), &mut formatter).await)
                                }
                            }
                        }
                    }

                    drop(formatter);

                    types::String::from(result).into()
                }
                types::PatternArgs { mut args } => {
                    let format_string = try_result!(args.next().ok_or("missing format string"));
                    let format_string = try_result!(ctx.eval_as::<types::String>(format_string).await);

                    let pos_args: Vec<_> = args.by_ref().collect();
                    let keyed_args = std::mem::take(&mut args.keyed);

                    try_result!(args.unused_arguments());

                    let (source, parts) = try_result!(format_string.map(|s| format_parts(s.as_ref().as_str()))
                                                                    .transpose().map_err(|e| e.into_error())).take();
                    #[derive(Debug, Clone)]
                    enum FormatBind {
                        String(String),
                        Value(Source<Value>),
                    }

                    impl AsDependency for FormatBind {
                        fn as_dependency(&self) -> Dependency {
                            match self {
                                FormatBind::String(s) => Dependency::hashed(&s),
                                FormatBind::Value(v) => Dependency::Value(v.value().clone())
                            }
                        }
                    }

                    let mut binds = Vec::new();
                    let mut last_was_bind = false;
                    for part in parts {
                        match part {
                            FormatPart::String(s) => {
                                binds.push(FormatBind::String(s));
                                last_was_bind = false;
                            }
                            other => {
                                if std::mem::replace(&mut last_was_bind, true) {
                                    return source.with("format string is undecidable: cannot have format arguments without a string literal between them")
                                        .into_error().into();
                                }
                                let (val, disp) = match other {
                                    FormatPart::Positional(p) => (pos_args.get(p), p.to_string()),
                                    FormatPart::Keyed(k) => (keyed_args.get(&crate::make_string(k.as_str())), k),
                                    _ => panic!("invalid format part")
                                };
                                match val {
                                    None => return source.with(format!("format string argument '{}' not found", disp)).into_error().into(),
                                    Some(v) => binds.push(FormatBind::Value(v.clone()))
                                }
                            }
                        }
                    }

                    let deps = depends![nsid!(std::string::format::pattern), ^@binds];

                    types::Unbound::new_no_doc(move |ctx, arg| {
                        let binds = binds.clone();
                        async move {
                            let (s_source, s) = try_result!(ctx.eval_as::<types::String>(arg).await).take();

                            let mut bind_strings: std::collections::BTreeMap<Source<Value>, Vec<String>> = Default::default();

                            let mut s = s.as_ref().as_str();
                            let mut binds = binds.into_iter();
                            while let Some(b) = binds.next() {
                                match b {
                                    FormatBind::String(string_literal) => {
                                        if let Some(0) = s.find(&string_literal) {
                                            s = &s[string_literal.len()..];
                                        } else {
                                            return s_source.with(format!("substring missing: {}", string_literal)).into_error().into();
                                        };
                                    }
                                    FormatBind::Value(v) => {
                                        match binds.next() {
                                            Some(FormatBind::String(string_literal)) => {
                                                if let Some(n) = s.find(&string_literal) {
                                                    bind_strings.entry(v).or_default().push(s[..n].to_owned());
                                                    s = &s[n+string_literal.len()..];
                                                } else {
                                                    return s_source.with(format!("substring missing: {}", string_literal)).into_error().into();
                                                }
                                            }
                                            Some(FormatBind::Value(_)) => panic!("invalid format bind state"),
                                            None => {
                                                bind_strings.entry(v).or_default().push(s.to_owned());
                                            }
                                        }
                                    }
                                }
                            }

                            for (k,v) in bind_strings {
                                if v.len() == 1 {
                                    try_result!(traits::bind_no_error(ctx, k, s_source.clone().with(crate::make_string(&v.into_iter().next().unwrap()))).await);
                                } else {
                                    let v = s_source.clone().with(types::Array(v.into_iter().map(|s| s_source.clone().with(crate::make_string(&s))).collect()).into());
                                    try_result!(traits::bind_no_error(ctx, k, v).await);
                                }
                            }

                            types::Unit.into()
                        }.boxed()
                    }, deps).into()
                }
                v => traits::bind_error(ctx, arg_source.with(v)).into()
            }
        }.boxed()
    }, depends![nsid!(std::string::format)],
    r#"Create or deconstruct a string based on a format specification.

Format strings may contain curly brackets to indicate arguments to insert/retrieve in the string.
The curly brackets may be empty (`{}`) or may contain a numeric index to indicate which positional
argument to use, or a string index to indicate which keyed argument to use. Empty brackets will use
the next positional argument unused by other empty brackets. Note that since the curly brackets are
syntactically-significant, a format string containing them will need to be quoted.  To insert a
literal open curly bracket, use `{{`, and likewise `}}` for a literal close curly bracket.

## Creation

Arguments: `(String :format-string) ^:arguments`

### Examples
* `format "hello {}" world` => `"hello world"`
* `format "hello {name}" (name = billy)` => `"hello billy"`
* `format "{}, {a}, {}, {0}, and {{{}}}" (a = alice) bob cal daisy` => `"bob, alice, cal, bob, and {daisy}"`

## Deconstruction

Pattern Arguments: `(String :format-string) ^:arguments`

If an argument is referenced more than once in the format string, the argument will be bound to an
Array of each String match.

Substring matches are determined by non-greedily matching string portions of the format string.

### Examples
* `format "hello {}" :name = "hello world"` => `name` is bound with `world`
* `format "hello {name}" ^{name} = "hello billy"` => `name` is bound with `billy`
* `format "{}, {a}, {}, {0}, {}" (a=:alice) :bob :cal (format "and {}" :daisy) = "billy, alice, cal, bob, and daisy"
  => bindings of `alice` -> `alice`, `bob` -> `[billy,bob]`, `cal` -> `cal`, `daisy` -> `daisy`"#).into()
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
        fn chars(t) {
            t.assert_content_eq("self:array:from <| self:string:chars hello", "[h,e,l,l,o]");
        }
    }

    ergo_script::test! {
        fn format_create(t) {
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
        fn format_match(t) {
            t.assert_content_eq(r#"self:string:format "hello {}" :world = "hello world""#, "{ world = world }");
            t.assert_content_eq(r#"self:string:format "{1} {} {2} {0}" :a :b :c = "1 2 3 4""#, "{a = [2,4], b = 1, c = 3}");
            t.assert_content_eq(r#"self:string:format "{named} {}" ^{named} :pos = "howdy pardner""#, "{ named = howdy, pos = pardner }");
            t.assert_content_eq(r#"self:string:format "{{ {} }}" :v = "{ hi }""#, "{ v = hi }");
            t.assert_fail(r#"bind (self:string:format "no match {}" :v -> :v) match"#);
            t.assert_fail(r#"bind (self:string:format "{} no" :v -> :v) str"#);
            t.assert_fail(r#"self:string:format "{}{}" :a :b = something"#);
            t.assert_fail(r#"self:string:format "{" = s"#);
            t.assert_fail(r#"self:string:format "}" = s"#);
            t.assert_fail(r#"self:string:format "{}" = s"#);
            t.assert_fail(r#"self:string:format "{named}" = s"#);
            t.assert_fail(r#"self:string:format "{{{}}" = s"#);
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
