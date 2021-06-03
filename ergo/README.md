# Effio ergo sum.

Ergo is a task runner akin to `make`, with dependency tracking that
extends to arbitrary values (not just files) and a language and runtime that
allows one to write tasks imperatively rather than explicitly specifying rules.

Features include:
* A hybrid-typed scripting language.
* A simple string-first syntax, like that of shell scripts.
* Script-level control of concurrent processing.
* Dependency tracking for all produced values.
* ABI-stability to support native plugins.

Planned features include:
* Persistent database of command statistics aggregated over multiple runs and
used to estimate future performance.

## Script Example

```sh
#!/usr/bin/env ergo

^ergo std

run_with_path = fn ^{^kwargs} ^args -> exec ^:kwargs ^{env = {PATH = env:get PATH}} ^:args |>:complete

# Get influxdb-cxx
influx = {
  checkout = seq ^[
      dir = (path:new)
      exec git clone https://github.com/awegrzyn/influxdb-cxx.git :dir |>:complete
      :dir
  ]
  include = path:join :checkout include
  builddir = seq ^[
      dir = (path:new)
      run_with_path cmake -DCMAKE_BUILD_TYPE=Release -S :checkout -B :dir
      :dir
  ]
  lib = value:cache <| seq ^[
      run_with_path ^{ pwd = :builddir } make InfluxDB
      path:join :builddir lib libInfluxDB.so
  ]

  { include, lib, libpath = path:join :builddir lib }
}

# Create test program
test = value:cache <| seq ^[
    out = (path:new)
    run_with_path c++ -std=c++17 -o :out -I influx:include (fs:track main.cpp) influx:lib
    :out
]

test-dist = path:join :script-dir influx-test

# Create output map
{
  dist = fs:copy :test :test-dist
  clean = exec rm -f :test-dist |>:complete
  test = exec ^{env = {LD_LIBRARY_PATH = influx:libpath}} :test
}
```

## Documentation
All values can have associated docstrings, which can be accessed with the `-d`
argument. This argument implies `-p`, using your system pager to print the
documentation. If `PAGER` is not set in the environment, this defaults to `less
-F`.

Unfortunately, it seems like older Mac versions have a buggy `less` which will
not work correctly with this flag. You can either:
* set `PAGER` to not use `-F`,
* set `PAGER` as `more -F` (which on mac is actually `less` in `LESS_IS_MORE` mode, but seems
  to work),
* install a newer version of `less` with brew and set `PAGER` to that, or
* add `-p` to _disable_ paging when displaying the documentation.

## Script Resolution
Project-mode is detected by the presence of a `workspace.ergo` file/directory
somewhere in the parent hierarchy. That directory is where project top-level
scripts reside.

## Development Notes

### TODO
* Display more detailed error information at exit?
* Extend the Ctrl-C behavior to better cancel/stop execution.
  * Right now it only cancels tasks.
* Persist command timing information for better estimates.
* Add value dependency tree print to help debug consistency issues.
* Change `path:new` to have identity based on source file and occurrence/seed
  value.
* Deduplicate values based on identity.
* Add `fs:expose` or something named similarly to expose read-only files.
  * Change `fs:copy` behavior to actually make copies (i.e., write-able files).
* Strongly control the lifetime of the thread pool using weak references in the
  grease runtime.
  * This will allow loaded plugins to be properly dropped rather than leaked.
* Improve std::fs file not found errors (print the file!).
* Investigate returning errors as a unique type (much like `Unset`). This would
  make catching and throwing errors more natural, and would make _caching_
  errors easy/automatic (more correctly persisting the state), though some
  design would have to go into handling persistent source information.
* Add a convenience function to turn a map into command-line flags (based on a
  specification of which flags have required args). This is convenient for
  things that want to pass flags to a program but also might need to easily
  inspect/manipulate the flags.
* Fix interaction of persistent caching and metadata (metadata not persisted).

### Optimizations
There are a few ways to _really_ speed up scripts that should be experimented
with in the runtime. Based on profiling, parsing is fast enough to completely
ignore for optimization (~500k characters per second on randomly mixed
expressions), so targetting evaluation is the natural next step. In particular,
function evaluation of cached values can and should be optimized. If a script
function's resulting type and identity can be determined based on inputs
(without actually executing the body), and/or there was a way for script
functions to declare these values, that would provide the ability for script
writers to hugely speed up evaluation (as large sections of script code could be
left unevaluated on a cache hit).

Such a change could be done in a number of ways, including:
* Adding a way for script writers to declare a function output relies only on
  inputs (this already exists if you make a function dynamically-typed, but
  could be more ergonomic).
* Partially evaluating functions to determine return type. If we didn't care
  about return type, it wouldn't matter as much (and maybe it would turn out to
  be okay to simply return dynamic values everywhere).
* Always delaying function calls (rather than calling them immediately if not
  dynamically typed, as it works now). Again, the implication of not having
  things typed isn't immediately clear; this _would_ cause type errors to not
  occur ASAP, but maybe that doesn't really matter as much as it might seem (as
  the error _would_ eventually occur, so really it just means that you could
  have erroneous code around that you don't know about until it is executed,
  which admittedly is something that many people complain about with interpreted
  dynamically-typed languages).
* Change evaluating to _always_ delay _everything_. This would be a fundamental
  change to how evaluation works. For instance, suppose a script sets a binding
  to a map that contains a few keys. With this approach, the original binding
  would be bound to a dynamically-typed value that, when forced later, would
  evaluate the map which _then_ would return a map that has keys that, when
  forced, would evaluate each binding. In essence, this is taking lazy
  evaluation to the extreme, _only_ evaluating any parsed values if they are
  used, which could leave a lot of errors unnoticed (which, in a
  lazily-evaluated language to begin with, might not be a huge deal).
  
  To clarify, here's the example written above as code and evaluation steps:
  ```
  my_map = {
     a = hello
     b = world
     c = string:format "{}, {}!" :a :b
  }
  ```
  evaluates to an environment containing `my_map`, where the stored `Value` is
  dynamic and will evaluate the literal parsed expression `{ a ... b ... c ...
  }` as above. Then, if you had
  ```
  my_map:a
  ```
  it would evaluate the parsed expression to a map which contains `a`, `b`, and
  `c`, but each would again have values that are dynamically-typed `Value`
  that evaluate `hello`, `world`, and `string:format "{}, {}!" :a :b`
  respectively (though an easy optimization would be to just immediately
  evaluate strings, no real overhead there).

  Such an approach could also be used to have better inter-file references
  without reducing circular reference detection to the granularity of files.
  Though the evaluation could be changed in this way to allow circular file
  references _without_ changing how the script runtime works, too (i.e. doing
  the above but making sure all script code ends up evaluated prior to forcing
  the final `Value`).

  Also, if everything were delayed in this matter, writing `if` and similar
  things as functions would be very easy. Otherwise the only way for those to be
  functions would be to add syntax which essentially does this (takes any
  expression and makes it a dynamically-typed value which will evaluate it).


Id generation is also a hot spot. If the hashing proves to be too cumbersome, at
the expense of memory we could store everything _to_ be hashed, and only
generate the effective identity when necessary.
