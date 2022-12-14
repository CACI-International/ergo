//! Logging functions.

use ergo_runtime::{context::Log, metadata::Source, nsid, traits, types, Context, Value};

pub fn function() -> Value {
    logger(Context::global().log.clone())
}

fn logger(log: Log) -> Value {
    types::unbound_value! {
        #![depends(const nsid!(std::log))]
        let ind = Context::eval_as::<types::Index>(ARG).await?
            .into_owned()
            .0;
        let ind = Context::eval_as::<types::String>(ind).await?;

        let log = log.clone();

        let s = ind.as_ref().as_str();
        if s == "sublog" {
            types::ergo_fn_value! {
                /// Create a sublogger from this logger.
                ///
                /// Arguments: `(String :name)`
                async fn sublog(name: types::String) -> Value {
                    logger(log.sublog(name.as_ref().as_str()))
                }
            }
        } else if s == "debug" {
            types::ergo_fn_value! {
                /// Display a value in the debug log.
                ///
                /// Arguments: `:value`
                async fn debug(value: _) -> Value {
                    let mut s = String::new();
                    {
                        let mut formatter = traits::Formatter::new(&mut s);
                        traits::display(value, &mut formatter).await?;
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
                async fn info(value: _) -> Value {
                    let mut s = String::new();
                    {
                        let mut formatter = traits::Formatter::new(&mut s);
                        traits::display(value, &mut formatter).await?;
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
                async fn warn(value: _) -> Value {
                    let mut s = String::new();
                    {
                        let mut formatter = traits::Formatter::new(&mut s);
                        traits::display(value, &mut formatter).await?;
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
                async fn error(value: _) -> Value {
                    let mut s = String::new();
                    {
                        let mut formatter = traits::Formatter::new(&mut s);
                        traits::display(value, &mut formatter).await?;
                    }
                    log.error(s);
                    types::Unit.into()
                }
            }
        } else {
            Source::get(&ind).with("unknown index").into_error().into()
        }
    }
}
