//! Network module.

use ergo_runtime::abi_stable::{bst::BstMap, type_erase::Erased, StableAbi};
use ergo_runtime::{
    context::ItemContent,
    depends,
    error::{Diagnostic, DiagnosticInfo},
    io,
    metadata::Source,
    nsid, traits, try_result,
    type_system::ErgoType,
    types, Context, Dependencies, Value,
};
use reqwest::{
    blocking::Client,
    header::{HeaderMap, HeaderName, HeaderValue},
    Method, StatusCode,
};
use std::convert::TryFrom;

pub fn module() -> Value {
    crate::make_string_map! {
        "http" = http(),
        "url-encode" = url_encode()
    }
}

#[types::ergo_fn]
/// Perform an HTTP request.
///
/// Arguments: `(String :url)`
///
/// Keyed Arguments:
/// * `default (String :method) as get`: The HTTP method to use. May be one of `get`, `post`,
/// `put`, `delete`, `head`, `options`, `connect`, `patch`, or `trace`.
/// * `String :basic-auth`: Use HTTP Basic Authentication. The string contains the username, and
/// optionally a colon followed by the password.
/// * `String :bearer-auth`: Use HTTP Bearer Authentication with the given token.
/// * `Into<Number> :timeout`: A timeout to use, in seconds.
/// * `(Map:Of :String :String) :headers`: Key-value pairs are header names and values to set for the request.
/// * `Into<ByteStream> :body`: The body to send with the request, if any.
///
/// Returns a Map with the following indices:
/// * `:body`: The response body, as a `ByteStream`.
/// * `:status-code`: The HTTP response status code, as an `HttpStatus`.
/// * `:headers`: The response headers, as a map of String to ByteStream. The keys are always
/// lowercase.
/// * `:complete`: Waits for the response to complete with a successful status code, evaluating to
/// `Unit` or an `Error`.
///
/// The `HttpStatus` type can be converted to `Bool` to check whether the returned status code
/// indicates success or not.
async fn http(
    url: types::String,
    (method): [types::String],
    (basic_auth): [types::String],
    (bearer_auth): [types::String],
    (timeout): [_],
    (headers): [types::Map],
    (body): [_],
) -> Value {
    let method = match method {
        None => Method::GET,
        Some(m) => match m.as_ref().as_str() {
            "get" => Method::GET,
            "post" => Method::POST,
            "put" => Method::PUT,
            "delete" => Method::DELETE,
            "head" => Method::HEAD,
            "options" => Method::OPTIONS,
            "connect" => Method::CONNECT,
            "patch" => Method::PATCH,
            "trace" => Method::TRACE,
            s => Err(Diagnostic::from(format!("invalid method: {}", s))
                .add_primary_label(Source::get(&m).with("")))?,
        },
    };

    let log = Context::global().log.sublog("net:http");

    let client = Client::builder().build()?;
    let mut request = client.request(method, url.as_ref().as_str());

    if let Some(auth) = basic_auth {
        let auth = auth.as_ref().as_str();
        match auth.find(':') {
            Some(ind) => {
                let (username, password) = auth.split_at(ind);
                request = request.basic_auth(username, Some(&password[1..]));
            }
            None => request = request.basic_auth::<&str, &str>(auth, None),
        }
    }

    if let Some(auth) = bearer_auth {
        let auth = auth.as_ref().as_str();
        request = request.bearer_auth(auth);
    }

    if let Some(to) = timeout {
        let to = traits::into::<types::Number>(to).await?;
        let secs = match to.as_ref().to_f64() {
            None => Err(Diagnostic::from("invalid timeout").add_primary_label(
                Source::get(&to).with("could not convert this to a floating point value"),
            ))?,
            Some(f) => f,
        };
        request = request.timeout(std::time::Duration::from_secs_f64(secs));
    }

    {
        let mut http_headers = HeaderMap::new();
        if let Some(headers) = headers {
            drop(traits::eval_nested(headers.clone().into()).await); // Eval in parallel
            for (k, v) in headers.to_owned().0.into_iter() {
                let k = Context::eval_as::<types::String>(k).await?;
                let v = Context::eval_as::<types::String>(v).await?;

                let k = HeaderName::try_from(k.as_ref().as_str()).add_primary_label(
                    Source::get(&k).with("while converting this value to a header name"),
                )?;
                let v = HeaderValue::try_from(v.as_ref().as_str()).add_primary_label(
                    Source::get(&v).with("while converting this value to a header value"),
                )?;
                http_headers.insert(k, v);
            }
        }
        request = request.headers(http_headers);
    }

    if let Some(body) = body {
        let body = traits::into::<types::ByteStream>(body).await?.to_owned();
        let mut body_vec = vec![];
        // TODO support streaming body
        io::copy(&mut body.read(), &mut io::AllowStdIo::new(&mut body_vec)).await?;
        request = request.body(body_vec);
    }

    log.debug(format!("sending http request: {:?}", request));

    // Wrap the request in a mutex because RequestBuilder is not Sync and cannot be captured in
    // `spawn_blocking`, but `Mutex` will be Sync.
    let req = std::sync::Mutex::new(request);
    let response = Context::global()
        .task
        .spawn_blocking(move || {
            req.into_inner()
                .unwrap()
                .send()
                .map_err(|e| ergo_runtime::error! { error: e })
        })
        .await
        .and_then(|v| v)?;

    log.debug(format!("got http response: {:?}", response));

    let err_status = response.status();
    let status = HttpStatus::from(response.status());

    let mut headers = BstMap::new();
    for (key, value) in response.headers() {
        let key: Value = types::String::from(key.as_str()).into();
        let value = Value::constant_deps(
            types::ByteStream::from(value.as_bytes().to_vec()),
            depends![^CALL_DEPENDS.clone(), nsid!(net::http::headers), key],
        );
        headers.insert(
            Source::imbue(ARGS_SOURCE.clone().with(key)),
            Source::imbue(ARGS_SOURCE.clone().with(value)),
        );
    }
    let headers = types::Map(headers);

    let body = Value::constant_deps(
        types::ByteStream::new(io::Blocking::new(response)),
        depends![^CALL_DEPENDS.clone(), nsid!(net::http::body)],
    );

    let complete = if err_status.is_success() {
        types::Unit.into()
    } else {
        let src = ARGS_SOURCE.clone();
        let body = body.clone();
        // TODO should this just immediately be an Error type?
        Value::dyn_new(
            move || async move {
                src.with(format!(
                    "{}: {}",
                    err_status,
                    try_result!(traits::to_string(body).await)
                ))
                .into_error()
                .into()
            },
            depends![^CALL_DEPENDS.clone(), nsid!(net::http::complete)],
        )
    };

    crate::make_string_map! { source ARGS_SOURCE,
        "status-code" = status.into(),
        "headers" = headers.into(),
        "body" = body,
        "complete" = complete
    }
}

/// The Http response status code.
#[derive(Clone, Debug, Eq, PartialEq, Hash, ErgoType, StableAbi)]
#[repr(C)]
pub struct HttpStatus(pub u16);

impl HttpStatus {
    pub fn success(&self) -> bool {
        StatusCode::from_u16(self.0)
            .map(|v| v.is_success())
            .unwrap_or(false)
    }
}

impl std::fmt::Display for HttpStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match StatusCode::from_u16(self.0) {
            Ok(sc) => sc.fmt(f),
            Err(_) => write!(f, "unknown code {}", self.0),
        }
    }
}

impl From<StatusCode> for HttpStatus {
    fn from(s: StatusCode) -> Self {
        HttpStatus(s.as_u16())
    }
}

impl From<HttpStatus> for types::Bool {
    fn from(e: HttpStatus) -> Self {
        types::Bool(e.success())
    }
}

impl From<HttpStatus> for types::String {
    fn from(e: HttpStatus) -> Self {
        types::String(e.0.to_string().into())
    }
}

impl From<HttpStatus> for ergo_runtime::TypedValue<HttpStatus> {
    fn from(e: HttpStatus) -> Self {
        ergo_runtime::TypedValue::constant(e)
    }
}

impl From<&'_ HttpStatus> for Dependencies {
    fn from(e: &'_ HttpStatus) -> Self {
        depends![HttpStatus::ergo_type(), e]
    }
}

ergo_runtime::HashAsDependency!(HttpStatus);

ergo_runtime::type_system::ergo_traits_fn! {
    traits::IntoTyped::<types::Bool>::add_impl::<HttpStatus>(traits);
    traits::IntoTyped::<types::String>::add_impl::<HttpStatus>(traits);
    ergo_runtime::ergo_display_basic!(traits, HttpStatus);
    ergo_runtime::ergo_type_name!(traits, HttpStatus);
    traits::ValueByContent::add_impl::<HttpStatus>(traits);

    impl traits::Stored for HttpStatus {
        async fn put(&self, _ctx: &traits::StoredContext, mut into: ItemContent) -> ergo_runtime::RResult<()> {
            bincode::serialize_into(&mut into, &self.0)
                .add_primary_label(Source::get(SELF_VALUE).with("while storing this value"))
                .map_err(|e| e.into())
                .into()
        }

        async fn get(_ctx: &traits::StoredContext, mut from: ItemContent) -> ergo_runtime::RResult<Erased>
        {
            bincode::deserialize_from(&mut from)
                .map(|status| Erased::new(HttpStatus(status)))
                .into_diagnostic()
                .map_err(|e| e.into())
                .into()
        }
    }
}

#[types::ergo_fn]
/// Encode a string as a url component.
///
/// Arguments: `(Into<String> :s)`
///
/// Returns a url-encoded string.
async fn url_encode(s: _) -> Value {
    let s = traits::into::<types::String>(s).await?;
    types::String::from(
        percent_encoding::utf8_percent_encode(
            s.as_ref().as_str(),
            percent_encoding::NON_ALPHANUMERIC,
        )
        .to_string(),
    )
    .into()
}

#[cfg(test)]
mod test {
    use httpmock::{Method, MockServer};

    ergo_script::tests! {
        fn http_get(t) {
            let server = MockServer::start();
            server.mock(|when, then| {
                when.method(Method::GET)
                    .path("/hi");
                then.status(200)
                    .header("myheader", "42")
                    .body("hello world");
            });
            t.assert_content_eq(&format!(r#"self:net:http "{}" |>:complete"#, server.url("/hi")), "()");
            t.assert_content_eq(&format!(r#"self:net:http "{}" |>:headers:myheader | self:string:from"#, server.url("/hi")), "42");
            t.assert_content_eq(&format!(r#"self:net:http "{}" |>:body | self:string:from"#, server.url("/hi")), "\"hello world\"");
            t.assert_content_eq(&format!(r#"self:net:http "{}" |>:status-code | self:string:from"#, server.url("/hi")), "200");
        }

        fn http_basic_auth(t) {
            let server = MockServer::start();
            server.mock(|when, then| {
                when.method(Method::GET)
                    .path("/auth")
                    .header("Authorization", "Basic YWxhZGRpbjpvcGVuc2VzYW1l");
                then.status(200);
            });
            t.assert_content_eq(&format!(r#"self:net:http (basic-auth = "aladdin:opensesame") "{}" |>:complete"#, server.url("/auth")), "()");
        }

        fn http_bearer_auth(t) {
            let server = MockServer::start();
            server.mock(|when, then| {
                when.method(Method::GET)
                    .path("/auth")
                    .header("Authorization", "Bearer 12345");
                then.status(200);
            });
            t.assert_content_eq(&format!(r#"self:net:http (bearer-auth = 12345) "{}" |>:complete"#, server.url("/auth")), "()");
        }

        fn http_put(t) {
            let server = MockServer::start();
            server.mock(|when, then| {
                when.method(Method::PUT)
                    .path("/put")
                    .body("mydata");
                then.status(200);
            });
            t.assert_fail(&format!(r#"self:net:http (body=mydata) "{}" |>:complete"#, server.url("/put")));
            t.assert_fail(&format!(r#"self:net:http (method=put) (body=mydataa) "{}" |>:complete"#, server.url("/put")));
            t.assert_content_eq(&format!(r#"self:net:http (method=put) (body=mydata) "{}" |>:complete"#, server.url("/put")), "()");
        }

        fn http_headers(t) {
            let server = MockServer::start();
            server.mock(|when, then| {
                when.method(Method::GET)
                    .path("/hdr")
                    .header("myheader", "42");
                then.status(200);
            });

            t.assert_fail(&format!(r#"self:net:http (headers={{myheader = 43}}) "{}" |>:complete"#, server.url("/hdr")));
            t.assert_content_eq(&format!(r#"self:net:http (headers={{myheader = 42}}) "{}" |>:complete"#, server.url("/hdr")), "()");
        }

        fn http_timeout(t) {
            let server = MockServer::start();
            server.mock(|when, then| {
                when.method(Method::GET)
                    .path("/onesecond");
                then.delay(std::time::Duration::from_secs(1)).status(200);
            });

            t.assert_fail(&format!(r#"self:net:http (timeout=1/10) "{}" |>:complete"#, server.url("/onesecond")));
        }
    }
}
