# Bob Gets Rusty
This repository contains a bob core runtime implementation in rust. It has
diverged from `bob`'s limitations, allowing arbitrary program execution.

The project includes a few separate rust crates:
* [`so`](so) - The main executable, using the other crates.
* [`so-repo`](so-repo) - A web server allowing `so repo` commands to be run
  automatically, triggered by repository state changes.
* [`exec`](exec) - External executable launching and configuration.
* [`grease`](grease) - A plan definition and runtime library.
* [`grease_macro`](grease_macro) - Procedural macros exposed by `grease`.
* [`tack`](tack) - A plugin interface.

