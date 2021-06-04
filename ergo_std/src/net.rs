//! Network module.

use ergo_runtime::{traits, try_result, types, Value};
use reqwest::{
    blocking::Client,
    header::{HeaderMap, HeaderName, HeaderValue},
};
use std::convert::TryFrom;

pub fn module() -> Value {
    crate::make_string_map! {
        "download" = download()
    }
}

#[types::ergo_fn]
/// Download a file over HTTP or HTTPS.
///
/// Arguments: `(String :url) (Path :target)`
///
/// Keyed Arguments:
/// * `(Map:Of :String :String) :headers`: Key-value pairs are header names and values to set for the request.
///
/// Downloads the file referenced by `url` to the given `target`, returning a Unit value.
async fn download(url: types::String, path: types::Path, (headers): [types::Map]) -> Value {
    let mut http_headers = HeaderMap::new();
    if let Some(headers) = headers {
        let headers = headers.unwrap();
        traits::eval_nested(CONTEXT, headers.clone().into()).await;
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

    let client = Client::new();
    let mut response = try_result!(client
        .get(url.value().as_ref().as_str())
        .headers(http_headers)
        .send()
        .and_then(|r| r.error_for_status()));

    let mut f = try_result!(std::fs::File::create(path.value().as_ref().as_ref()));
    try_result!(std::io::copy(&mut response, &mut f));

    types::Unit.into()
}
