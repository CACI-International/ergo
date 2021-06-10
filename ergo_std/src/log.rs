//! Logging functions.

use ergo_runtime::{context::Log, depends, nsid, traits, try_result, types, Context, Value};
use futures::FutureExt;

pub fn function(ctx: &Context) -> Value {
    logger(ctx.log.clone())
}

fn logger(log: Log) -> Value {
    types::Unbound::new_no_doc(
        move |ctx, arg| {
            let log = log.clone();
            async move {
                let ind = try_result!(ctx.eval_as::<types::Index>(arg).await).unwrap().to_owned().0;
                let (src, ind) = try_result!(ctx.eval_as::<types::String>(ind).await).take();

                let log = log.clone();

                let s = ind.as_ref().as_str();
                if s == "sublog" {
                    types::ergo_fn_value! {
                        /// Create a sublogger from this logger.
                        ///
                        /// Arguments: `(String :name)`
                        #[cloning(log)]
                        async fn sublog(name: types::String) -> Value {
                            logger(log.sublog(name.value().as_ref().as_str()))
                        }
                    }
                } else if s == "debug" {
                    types::ergo_fn_value! {
                        /// Display a value in the debug log.
                        ///
                        /// Arguments: `:value`
                        #[cloning(log)]
                        async fn debug(value: _) -> Value {
                            let mut s = String::new();
                            {
                                let mut formatter = traits::Formatter::new(&mut s);
                                try_result!(traits::display(CONTEXT, value.unwrap(), &mut formatter).await);
                            }
                            log.debug(s);
                            types::Unit.into()
                        }
                    }
                } else if s == "info" {
                    types::ergo_fn_value! {
                        /// Display a value in the info log.
                        ///
                        /// Arguments: `:value`
                        #[cloning(log)]
                        async fn info(value: _) -> Value {
                            let mut s = String::new();
                            {
                                let mut formatter = traits::Formatter::new(&mut s);
                                try_result!(traits::display(CONTEXT, value.unwrap(), &mut formatter).await);
                            }
                            log.info(s);
                            types::Unit.into()
                        }
                    }
                } else if s == "warn" {
                    types::ergo_fn_value! {
                        /// Display a value in the warn log.
                        ///
                        /// Arguments: `:value`
                        #[cloning(log)]
                        async fn warn(value: _) -> Value {
                            let mut s = String::new();
                            {
                                let mut formatter = traits::Formatter::new(&mut s);
                                try_result!(traits::display(CONTEXT, value.unwrap(), &mut formatter).await);
                            }
                            log.warn(s);
                            types::Unit.into()
                        }
                    }
                } else if s == "error" {
                    types::ergo_fn_value! {
                        /// Display a value in the error log.
                        ///
                        /// Arguments: `:value`
                        #[cloning(log)]
                        async fn error(value: _) -> Value {
                            let mut s = String::new();
                            {
                                let mut formatter = traits::Formatter::new(&mut s);
                                try_result!(traits::display(CONTEXT, value.unwrap(), &mut formatter).await);
                            }
                            log.error(s);
                            types::Unit.into()
                        }
                    }
                }
                else {
                    src.with("unknown index").into_error().into()
                }
            }.boxed()
        },
        depends![nsid!(std::log)],
    )
    .into()
}
