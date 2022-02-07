# Ergo
This repository contains related projects for the `ergo` language. See the
[`ergolang` README](ergolang/README.md) for more details.

The project includes a few separate rust crates:
* [`ergolang`](ergolang) - The main executable, using the other crates.
* [`abi_stable_ext`](abi_stable_ext) - Extensions to the `abi_stable` crate.
* [`ergo_runtime`](ergo_runtime) - The ergo runtime, containing the ergo type
  system, trait system, basic types, basic traits, and runtime context.
* [`ergo_runtime_macro`](ergo_runtime_macro) - Procedural macros exposed by
  `ergo_runtime`.
* [`ergo_script`](ergo_script) - The ergo script crate, containing script
  parsing and evaluation functionality.
* [`ergo_std`](ergo_std) - The ergo standard library plugin.

