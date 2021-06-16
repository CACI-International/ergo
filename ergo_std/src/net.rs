//! Network module.

use ergo_runtime::abi_stable::{bst::BstMap, StableAbi};
use ergo_runtime::{
    depends, io, nsid, traits, try_result, type_system::ErgoType, types, Dependencies, Error, Value,
};
use reqwest::{
    blocking::Client,
    header::{HeaderMap, HeaderName, HeaderValue},
    Method, StatusCode,
};
use std::convert::TryFrom;
use std::str::FromStr;

pub fn module() -> Value {
    crate::make_string_map! {
        "http" = http()
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
/// * `String :timeout`: A timeout number to use, in milliseconds.
/// * `(Map:Of :String :String) :headers`: Key-value pairs are header names and values to set for the request.
/// * `Into<ByteStream> :body`: The body to send with the request, if any.
///
/// Returns a Map with the following indices:
/// * `:body`: The response body, as a `ByteStream`.
/// * `:status-code`: The HTTP response status code, as an `HttpStatus`.
/// * `:headers`: The response headers, as a map of String to ByteStream.
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
    (timeout): [types::String],
    (headers): [types::Map],
    (body): [_],
) -> Value {
    let method = match method {
        None => Method::GET,
        Some(m) => match m.value().as_ref().as_str() {
            "get" => Method::GET,
            "post" => Method::POST,
            "put" => Method::PUT,
            "delete" => Method::DELETE,
            "head" => Method::HEAD,
            "options" => Method::OPTIONS,
            "connect" => Method::CONNECT,
            "patch" => Method::PATCH,
            "trace" => Method::TRACE,
            s => {
                return m
                    .source()
                    .with(format!("invalid method: {}", s))
                    .into_error()
                    .into()
            }
        },
    };

    let log = CONTEXT.log.sublog("net:http");

    let client = Client::new();
    let mut request = client.request(method, url.value().as_ref().as_str());

    if let Some(auth) = basic_auth {
        let auth = auth.value().as_ref().as_str();
        match auth.find(':') {
            Some(ind) => {
                let (username, password) = auth.split_at(ind);
                request = request.basic_auth(username, Some(password));
            }
            None => request = request.basic_auth::<&str, &str>(auth, None),
        }
    }

    if let Some(auth) = bearer_auth {
        let auth = auth.value().as_ref().as_str();
        request = request.bearer_auth(auth);
    }

    if let Some(to) = timeout {
        let (to_source, to) = to.take();
        let to = to.as_ref().as_str();
        let millis = match u64::from_str(to) {
            Err(_) => return to_source.with("invalid numeric string").into_error().into(),
            Ok(s) => s,
        };
        request = request.timeout(std::time::Duration::from_millis(millis));
    }

    {
        let mut http_headers = HeaderMap::new();
        if let Some(headers) = headers {
            let headers = headers.unwrap();
            drop(traits::eval_nested(CONTEXT, headers.clone().into()).await);
            for (k, v) in headers.to_owned().0.into_iter() {
                let k = try_result!(CONTEXT.eval_as::<types::String>(k).await);
                let v = try_result!(CONTEXT.eval_as::<types::String>(v).await);

                let k = try_result!(k
                    .map(|k| HeaderName::try_from(k.as_ref().as_str()))
                    .transpose_err());
                let v = try_result!(v
                    .map(|v| HeaderValue::try_from(v.as_ref().as_str()))
                    .transpose_err());
                http_headers.insert(k, v);
            }
        }
        request = request.headers(http_headers);
    }

    if let Some(body) = body {
        let body = try_result!(traits::into_sourced::<types::ByteStream>(CONTEXT, body).await)
            .unwrap()
            .to_owned();
        let mut body_vec = vec![];
        // TODO support streaming body
        try_result!(
            io::copy(
                &CONTEXT.task,
                &mut body.read(),
                &mut io::Writer(&mut body_vec)
            )
            .await
        );
        request = request.body(body_vec);
    }

    log.debug(format!("sending http request: {:?}", request));

    // Wrap the request in a mutex because RequestBuilder is not Sync and cannot be captured in
    // `spawn_blocking`, but `Mutex` will be Sync.
    let req = std::sync::Mutex::new(request);
    let response = try_result!(CONTEXT
        .task
        .spawn_blocking(move || req.into_inner().unwrap().send().map_err(Error::from))
        .await
        .and_then(|v| v));

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
            ARGS_SOURCE.clone().with(key),
            ARGS_SOURCE.clone().with(value),
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
        Value::dyn_new(
            move |ctx| async move {
                src.with(format!(
                    "{}: {}",
                    err_status,
                    try_result!(traits::to_string(ctx, body).await)
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
