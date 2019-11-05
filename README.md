# Bob Gets Rusty
This repository contains a bob core runtime implementation in rust.

The project includes a few separate rust crates:
* [`bob`](bob) - The main bob executable, using the other crates.
* [`exec`](exec) - External executable launching and configuration.
* [`grease`](grease) - A plan definition and runtime library.
* [`grease_macro`](grease_macro) - Procedural macros exposed by `grease`.
* [`tack`](tack) - A plugin interface.

