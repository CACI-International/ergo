//! Network module.

use ergo_runtime::{ergo_function, source_value_as, traits, types};
use grease::{bst::BstMap, depends, make_value, path::PathBuf, value::Value};
use reqwest::{
    header::{HeaderMap, HeaderName, HeaderValue},
    Client,
};

pub fn module() -> Value {
    let mut map = BstMap::new();
    map.insert("download".into(), download_fn());
    types::Map(map).into()
}

fn download_fn() -> Value {
    ergo_function!(std::net::download, |ctx| {
        let url = ctx.args.next().ok_or("no url provided")?;
        let path = ctx.args.next().ok_or("no path provided")?;

        let url = source_value_as!(url, types::String, ctx)?.unwrap();
        let path = source_value_as!(path, PathBuf, ctx)?.unwrap();

        let headers = match ctx.args.kw("headers") {
            None => None,
            Some(v) => Some(source_value_as!(v, types::Map, ctx)?),
        };

        ctx.unused_arguments()?;

        let mut deps = depends![url, path];
        if let Some(v) = &headers {
            deps += depends![**v];
        }

        let rctx = ctx.clone();

        make_value!([^deps] {
            let (url,path) = rctx.task.join(url, path).await?;

            let mut http_headers = HeaderMap::new();
            if let Some(headers) = headers {
                let (headers_source, headers) = headers.take();
                traits::force_value_nested(&rctx.traits, headers.clone().into()).await?;
                for (k,v) in headers.forced_value().0.iter() {
                    let v = source_value_as!(headers_source.clone().with(v.clone()), types::String, rctx)?.unwrap().await?;
                    http_headers.insert(HeaderName::from_bytes(k.as_bytes())?,
                        HeaderValue::from_str(v.as_ref())?);
                }
            }

            let client = Client::new();
            let mut response = client.get(url.as_str()).headers(http_headers).send().await?.error_for_status()?;

            let mut f = std::fs::File::create(path.as_ref().as_ref())?;
            while let Some(chunk) = response.chunk().await? {
                use std::io::Write;
                f.write_all(chunk.as_ref())?;
            }

            Ok(())
        })
        .into()
    })
    .into()
}
