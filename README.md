# Ergo

Ergo is a general purpose task-runner, with a supporting scripting language,
runtime, and standard library. As a tool, it fills a similar niche to GNU make,
but without as many limitations imposed on the user. It is intended to be used
for process automation and build systems, both at small and large scales. The
language and runtime are intended to be fast enough such that writing
significant amounts of functionality as scripts will be performant, however the
runtime supports native plugins if necessary (and the standard library is
implemented as such).

The ergo language semantics have been purpose-built with task-running and
build-systems in mind, and the syntax is meant to be largely approachable and
familiar, borrowing some components from other scripting languages, including
shell languages. The genesis and development of the language was driven by a
desire to improve build systems, in flexibility, expressiveness, and
correctness. A new language was necessary because no existing scripting language
could facilitate the key requirements of ergo: lazy evaluation and identity
derivation, treating your script and runtime values as a merkle tree to be able
to detect changes and prune unnecessary branches of script code without ever
evaluating them.

Ergo achieves its goals with a formal type system including type traits, lazy
evaluation, immutable, dynamically-typed values, and familiar, simple syntax. A
featureful standard library provides many useful tools, including the ability to
use remote ergo libraries, have per-user network authentication, access to
built-in caching at various levels (project/user/system/etc), logging, and
concurrent tasks. LSP support allows editors to have syntax highlighting and
formatting, and in the future, documentation, debugging and profiling support.
First-class documentation support for CLI docs as well as generated HTML docs
allows users to easily obtain technical and usage documentation.

The language shares many of the same goals and runtime features as
[Unison](unison-lang.org), though its design is meant to be more approachable
(and we didn't know about Unison until recently!).

## Script Example

The below example highlights a few different features of the language and
standard library, including:
* Running external programs.
* The ability to cache arbitrary values (which will appropriately evaluate or elide
  values on subsequent runs).
* Interpolated (templated/programmatic) strings and doc comments.

These are just a handful of the many available features.

```sh
#!/usr/bin/env ergo

std:import { :Path, :cache, :env, :exec, fs = {:track} } = $std

# Redefine exec to always use PATH (which will also depend on PATH if it changes)
exec = fn ^:args -> { exec ~env={PATH = env:vars:PATH} ^args; () }

# Get influxdb-cxx
influx = {
  checkout = Path:for <| fn :dir -> {
      exec git clone "https://github.com/awegrzyn/influxdb-cxx.git" $dir
  }
  include = Path:join $checkout include
  builddir = Path:for <| fn :dir -> {
      exec cmake -DCMAKE_BUILD_TYPE=Release -S $checkout -B $dir
  }
  lib = cache {
      exec ~pwd=$builddir make InfluxDB
      Path:join $builddir lib libInfluxDB.so
  }

  { include, lib, libpath = Path:join $builddir lib }
}

# Create a program that uses the downloaded influxdb.
my_prog = cache <| Path:for <| fn :out -> {
    exec c++ -std=c++17 -o $out -I influx:include (track main.cpp) influx:lib
}

test-dist = Path:join (std:script:dir ()) influx-test

commands = {
  ## build and copy outputs
  dist = fs:copy $my_prog $test-dist
  ## clean outputs
  clean = fs:remove $test-dist
  ## run the program
  run = exec ~env={LD_LIBRARY_PATH = influx:libpath} $my_prog
}

## Commands:
## $(std:String:join "\n" <| std:Iter:map (fn :entry -> "* $(entry:key) - $(doc entry:value)"))
fn :cmd -> commands:$cmd
```

## Language Novelties

While the scripting language has been designed to be fairly familiar looking,
there are a few somewhat novel features which may be confusing to users, and may
be of interest to programming language nerds.

### Setters are first-class values

When you write `foo = bar`, this is syntax sugar for `:foo = bar`. On the left
side of the `=`, `:foo` is evaluated to a _setter_. This setter will set the key
`foo` in the current binding scope (in the future, it may be possible to specify
in which scope you wish to set the value). Since values are immutable, this
setter may only be _bound_ once, and an error will occur if it is bound more
than once. The `=` operator is actually a binding operator, which evaluates the
left and right expressions, and binds the left with the right. Binding is a
runtime feature; any value may be bound with another (though whether this
results in an error or not is up to the value's type implementation).

Because setters are first-class values, you can write functions which operate on
them. For instance, in

```
std:String :foo = bar
```

The `std:String` function is simply called with the `:foo` setter as an
argument, and its implementation checks that the bound value is a `String` type
prior to then binding `:foo` with the value. See
[below](#binding-is-a-script-exposed-behavior) for how to write your own
functions like this!

### Anything can be a key in a binding

Though in most cases strings are keys when binding values (e.g., `foo` in `foo =
bar`), you may use any value as a key. The actual key used in the implementation
is the value's _identity_, so since all values provide an identity, all values
may be used. The syntax sugar expanding `foo = bar` to `:foo = bar` only applies
to a single basic string, so any other value requires the preceding `:`. For
instance:

```
:() = bar
:$unset = bar
:[a,b,c] = bar
x = foo
:$x = bar
```

### Lazy evaluation

Since the language is lazily and concurrently evaluated, script users need to
think about what they read/write a bit differently. For instance, a block

```
{
    foo 1 2 3
    [bar 1, bar 2, bar 3]
    baz 1 2 3
}
```

evaluates values in sequence, but it doesn't do this immediately! The above
block evaluates to a _lazy_ value which, _when needed_ will evaluate `foo 1 2 3`
fully, then each of `bar 1`, `bar 2`, `bar 3` _concurrently_, and then return
_another_ lazy value that will evaluate `baz 1 2 3` when needed. If that block
is never needed, the inner expressions will never be evaluated. This applies to
all compositional syntax primitives.

Lazy evaluation also means that conditional evaluation (which is normally
implemented as syntax keywords/primitives in other languages) can be implemented
as standard library functions. `if`, `match`, and others are implemented in the
standard library as normal functions!

### Binding is a script-exposed behavior

The feature of binding is available at a script level. The `->` operator creates
an _unbound_ value, where the value (when bound), will evaluate the left side of
the `->` and bind the original bound value to it in a newly-opened binding
scope, and will then evaluate the right side. For instance, you can write:

```
get-foo-key = fn :target -> std:Map :m -> { $target = m:foo; () }
get-foo-key :x = { foo = bar, abc = 123 }
```

First `get-foo-key` is called with the `:x` setter (binding the setter to
`:target`), then it is bound to the expression (and as implemented,
`get-foo-key` requires that value be a `std:Map` and binds it to `:m`), and the
resulting block is fully evaluated. After these two lines run, `x` in the
top-level scope is bound to the string `bar`.

This is fundamentally how `std:match` works, you pass an array of bindings to
try and it goes through each one:
```
std:match $value [
    specific-string -> ...
    std:String :any-string -> ...
    { :only-one-specific-key } -> ...
    std:Map :any-map -> ...
    [:first,:second,:third] -> ...
    [literal-string, ^:rest] -> ...
]
```

This allows for powerful and customizable matching of values.

### Function calls and indexing are bindings

As you might have seen above, function calls and indexing are implemented as
binding as well. When you write `foo 1 2 3`, the `foo` binding in scope is
_bound_ with an `Args`-typed value (containing positional and keyed arguments).
Because of this, `fn` is implemented as a function! When you write

```
fn :x :y -> ...
```

The left-hand side calls `fn` with the `:x` and `:y` setters. This simply
creates an `Args` value, which can be bound to another `Args` value (when the
function is called) to destructure and bind each argument. As such, you can
actually use `fn` to create `Args`-typed values directly if you wanted.

Likewise, indexing (e.g., `foo:bar`) binds an `Index`-typed value, and can be
matched using the built-in `index` function, e.g. `index :my-index -> ...`. Of
course, `Map` and `Array` types already implement binding `Index` values to get
their contents.

### Most syntax-level values are strings

Like shell languages and a few others, bare strings (without quotes) are
interpreted as `String`-typed values. The language uses a handful of syntax
sugar features to allow more cases to use bare strings as well. Quoted strings
are parsed during conventional tokenization, so you can use quoted strings for
pretty much anything. One thing that's fairly different here is that, unlike
most other languages, there is no tokenized (syntactic) number type. There _is_
a `Number` type (which covers arbitrary rational numbers) in the standard
library, and it simply reads the rational number from a `String` type (e.g.
`std:Number:from 1.25`, `std:Number:from -1/2`, etc).

### The language has no keywords

Perhaps not the most important feature, but the base ergo language is
spoken-language-agnostic. There are no keywords, only symbols. The built-in
functions/values and the standard library do generally use English words (or
abbreviations). But it is kind of cool (I think) to keep the base syntax purely
symbolic.


## Organization

The project includes a few separate rust crates:
* [`ergolang`](ergolang) - The main executable, using the other crates.
* [`ergo_abi_stable`](ergo_abi_stable) - Extensions to the `abi_stable` crate.
* [`ergo_runtime`](ergo_runtime) - The ergo runtime, containing the ergo type
  system, trait system, basic types, basic traits, and runtime context.
* [`ergo_runtime_macro`](ergo_runtime_macro) - Procedural macros exposed by
  `ergo_runtime`.
* [`ergo_script`](ergo_script) - The ergo script parsing and evaluation
  functionality.
* [`ergo_std`](ergo_std) - The ergo standard library plugin.

## Building

To build the code, use typical `cargo` commands. However, to build the
executable to be used, the `bootstrap.sh` script will build and arrange files in
the `dist` directory such that you can run `ergo`. The script forwards all
arguments to `cargo`. This is necessary because of the standard library native
plugin: `cargo` can't appropriately arrange the files where they need to be.

After bootstrapping, you can run `dist/bin/ergolang evaluate -d` to see
supported commands in the project's [workspace.ergo](workspace.ergo). Many of
these commands are not yet supported as they need to be updated to work with
GitHub resources.

## Documentation

A user guide exists as an mdbook in [user_guide](user_guide). The full guide can
be generated by running the `doc` command (e.g., `dist/bin/ergolang evaluate
doc` after bootstrapping).

### Runtime Documentation

All values can have associated docstrings, which can be accessed with the
`-d`/`--doc` argument. This argument implies `-p`/`--page`, using your system
pager to print the documentation. If `PAGER` is not set in the environment, this
defaults to `less -F`.

Unfortunately, it seems like older Mac versions have a buggy `less` which will
not work correctly with this flag, so the default is `less`. You can:
* set `PAGER` as `more -F` (which on mac is actually `less` in `LESS_IS_MORE` mode, but seems
  to work),
* install a newer version of `less` with brew and set `PAGER` to that, or
* add `-p` to _disable_ (toggle) paging when displaying the documentation.

