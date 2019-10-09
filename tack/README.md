# tack - plugin loading interface

`tack` is a rust plugin loading interface. It provides a C ABI-stable FFI,
however it is recommended to write both plugin loaders and plugins in rust, as
the library provides convenience interfaces.

To load plugins, enable the `registry` feature of the library and use the
`PluginRegistry` struct.

To write plugins, one should generally use the `register_plugin` macro to create
the plugin entrypoint. Plugins should generally be compiled as `cdylib` crates.

## Loading paradigm
Plugins entrypoints may return status about their loading state. They may signal
that they:
* successfully loaded
* are waiting on an optional extension dependency
* are waiting on a required extension dependency
* failed to load

The plugin registry will retry plugins which returned a waiting state when other
plugins have loaded or changed the registry. If plugins cannot be retried (due
to no changes in the registry) but have a required extension dependency, loading
will fail.

## Safety
Much like `std::sync::Arc` only ensures safe access to a reference, but not
necessarily the contents of the reference, so too does `tack` make access to
plugin extensions safe, but not necessarily using the extensions. Thus, care
must be taken to ensure plugin extensions are ABI-stable (using `repr(C)` types)
and, when applicable, are forward/backwards compatible.

