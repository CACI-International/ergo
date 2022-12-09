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
evaluation, dynamically-typed values, and familiar, simple syntax. A featureful
standard library provides many useful tools, including the ability to use remote
ergo libraries, have per-user network authentication, access to built-in caching
at various levels (project/user/system/etc), logging, and concurrent tasks. LSP
support allows editors to have syntax highlighting and formatting, and in the
future, documentation, debugging and profiling support. First-class
documentation support for CLI docs as well as generated HTML docs allows users
to easily obtain technical and usage documentation.

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

