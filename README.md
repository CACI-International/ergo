# Bob Gets Rusty
This repository contains a bob core runtime implementation in rust. It has
diverged from `bob`'s limitations, allowing arbitrary program execution.

The project includes a few separate rust crates:
* [`ergo`](ergo) - The main executable, using the other crates.
* [`ergo-repo`](ergo-repo) - A web server allowing `ergo repo` commands to be run
  automatically, triggered by repository state changes.
* [`ergo_runtime`](ergo_runtime) - The ergo runtime, building a type system with
  `grease`.
* [`ergo_runtime_macro`](ergo_runtime_macro) - Procedural macros exposed by
  `ergo_runtime`.
* [`ergo_std`](ergo_std) - The ergo standard library plugin.
* [`grease`](grease) - A plan definition and runtime library.
* [`grease_macro`](grease_macro) - Procedural macros exposed by `grease`.

