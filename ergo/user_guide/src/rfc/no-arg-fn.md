# No-argument functions

It has become clear that one should be able to call functions with no arguments.
Previously this was thought to be unnecessary since the lazily-evaluated runtime
obviates the need, however pragmatically it's not intuitive to not support this
for script writers (who may, for example, want some default behavior to occur if
a function is called without arguments), and there are some side-effectful
functions like `path new` which have different semantics and would benefit from
this (currently the entire `path` module is a function).

## Priorities
Make command-line syntax identical to script (e.g. `ergo script function` works
the same syntactically from command line or in another script). This requirement
precludes options like having an end-of-function-call indicator, which would be
noise on the command line (prone to user error).

## Option 1
* `map key args`: eval `key` function in map
* `map key`: eval `key` if function in map (no arguments)
* `key = fn ^args -> map key ^$args`: get function from map (this is awkward; improvements?)
  Maybe change map/array access so that `(map : key)` always returns the value?
* `(name)`: eval `name` if function (no arguments)
* `$name`: get `name` from environment

### Semantics
`(...)` and/or `a b c` (i.e. more than one string in sequence) will always
evaluate a function if one comes up.

## Option 2
Change map/array syntax.
* `map::key args`: eval `key` function in map
* `map::key`: eval `key` if function in map (no arguments)
* `$map::key`: get function/value from map
* `(name)`: eval `name` if function (no arguments)
* `$name`: get `name` from environment

### Semantics
`$[something]` will always return a named value, otherwise a function
is called.

This approach is not quite as command-line friendly (where command-line "syntax"
is always space-separated strings). E.g., `ergo build::debug library` vs `ergo
build debug library`, though if one desires the spaces-and-strings one could
always create a function rather than a map. This may improve script readability
by disambiguating function calls vs map/array accesses. This requires `::` to
become a token (but `:` should be excluded, so tokenizer will need to do some
lookahead).
