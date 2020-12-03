# Ergo
This repository contains related projects for the `ergo` task running program.
See the [`ergo` README](ergo/README.md) for more details.

The project includes a few separate rust crates:
* [`ergo`](ergo) - The main executable, using the other crates.
* [`ergo-repo`](ergo-repo) - A web server allowing `ergo repo` commands to be run
  automatically, triggered by repository state changes.
* [`ergo_runtime`](ergo_runtime) - The ergo runtime, building a type system with
  `grease`.
* [`ergo_runtime_macro`](ergo_runtime_macro) - Procedural macros exposed by
  `ergo_runtime`.
* [`ergo_std`](ergo_std) - The ergo standard library plugin.
* [`grease`](grease) - An abi-stable dynamic-typing runtime library and type
  trait system.
* [`grease_macro`](grease_macro) - Procedural macros exposed by `grease`.

