# Ergo
This repository contains related projects for the `ergo` task running program.
See the [`ergo` README](ergo/README.md) for more details.

The project includes a few separate rust crates:
* [`ergo`](ergo) - The main executable, using the other crates.
* [`ergo-repo`](ergo-repo) - A web server allowing `ergo repo` commands to be run
  automatically, triggered by repository state changes.
* [`abi_stable_ext`](abi_stable_ext) - Extensions to the `abi_stable` crate.
* [`ergo_runtime`](ergo_runtime) - The ergo runtime, containing the ergo type
  system, trait system, basic types, basic traits, and runtime context.
* [`ergo_runtime_macro`](ergo_runtime_macro) - Procedural macros exposed by
  `ergo_runtime`.
* [`ergo_script`](ergo_script) - The ergo script crate, containing script
  parsing and evaluation functionality.
* [`ergo_std`](ergo_std) - The ergo standard library plugin.

