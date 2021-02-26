//! Network module.

use ergo_runtime::{context_ext::AsContext, ergo_function, types, ContextExt};
use grease::{depends, make_value, path::PathBuf, value::Value};
use reqwest::{
    blocking::Client,
    header::{HeaderMap, HeaderName, HeaderValue},
};
use std::convert::TryFrom;

pub fn module() -> Value {
    crate::grease_string_map! {
        "download" = download_fn()
    }
}

fn download_fn() -> Value {
    ergo_function!(std::net::download,
    r"Download a file over HTTP or HTTPS.

Arguments: `(String :url) (Path :target)`

Keyword Arguments:
* `(Map:Of :String :String) :headers`: Key-value pairs are header names and values to set for the request.

Returns a unit-typed value which downloads the file referenced by `url` to the given `target`.",
    |ctx,args| {
        let url = args.next().ok_or("no url provided")?;
        let path = args.next().ok_or("no path provided")?;

        let url = ctx.source_value_as::<types::String>(url);
        let url = url.await?.unwrap();
        let path = ctx.source_value_as::<PathBuf>(path);
        let path = path.await?.unwrap();

        let headers = match args.kw("headers") {
            None => None,
            Some(v) => Some({
                let v = ctx.source_value_as::<types::Map>(v);
                v.await?
            }),
        };

        args.unused_arguments()?;

        let mut deps = depends![url, path];
        if let Some(v) = &headers {
            deps += depends![**v];
        }

        let rctx: grease::runtime::Context = ctx.as_context().clone();

        make_value!([^deps] {
            let (url,path) = rctx.task.join(url, path).await?;

            let mut http_headers = HeaderMap::new();
            if let Some(headers) = headers {
                let (headers_source, headers) = headers.take();
                rctx.force_value_nested(headers.clone().into()).await?;
                for (k,v) in headers.forced_value().0.iter() {
                    let k = rctx.source_value_as::<types::String>(headers_source.clone().with(k.clone())).await?.unwrap().await?;
                    let v = rctx.source_value_as::<types::String>(headers_source.clone().with(v.clone())).await?.unwrap().await?;
                    http_headers.insert(HeaderName::try_from(k.as_ref().as_str())?,
                        HeaderValue::try_from(v.as_ref().as_str())?);
                }
            }

            let client = Client::new();
            let mut response = client.get(url.as_str()).headers(http_headers).send()?.error_for_status()?;

            let mut f = std::fs::File::create(path.as_ref().as_ref())?;
            std::io::copy(&mut response, &mut f)?;

            Ok(types::Unit)
        })
        .into()
    })
    .into()
}
