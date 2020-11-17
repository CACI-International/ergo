# Effio ergo sum.

Ergo is a task runner akin to `make`, however with dependency tracking that
extends to arbitrary values, not just files and a language and runtime that
allows one to write tasks imperatively rather than explicitly specifying rules.

Features include:
* A hybrid-typed scripting language with string-first syntax, like that of shell
  scripts.
* Script-level control of concurrent processing.
* Dependency tracking for all produced values.
* ABI-stability to support native plugins.

Planned features include:
* Persistent database of command statistics aggregated over multiple runs and
used to estimate future performance.

## Script Example

```sh
#!/usr/bin/env ergo

^(ergo std)

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

## Script Resolution
Project-mode is detected by the presence of a `workspace.ergo` file/directory
somewhere in the parent hierarchy. That directory is where project top-level
scripts reside.

Within a script, the following are defined:
* `script-dir`: this is the directory that contains the currently-executing
  script.
* `load-path`: an array of paths that are
  used when resolving `ergo` calls to load external scripts. In any given script,
  it will by default contain the `script-dir` directory. An accompanything
  `share/ergo/lib` directory is also used if the location of the `ergo` binary
  is in a sibling `bin` directory. The application's user-local directory will
  also be added:
  * __Linux__: `$XDG_CONFIG_HOME/ergo/lib` or `$HOME/.local/share/ergo/lib`
  * __macOS__: `$HOME/Library/Application Support/ergo/lib`
  * __Windows__: `{FOLDERID_LocalAppData}\ergo\data\lib`

## Development Notes

### TODO
* Self-documentation. Accessed and printed directly while evaluating scripts
  and/or accessed with a `--doc` command-line argument. Add a way to attach
  documentation (or other metadata?) to arbitrary types in scripts.
* Display more detailed error information at exit?
* Add a message/signal handler to exit gracefully.
  * This has been added but child programs still capture the signal, need to
    change the process group to prevent this.
* Persist command timing information for better estimates.
* Allow functions to get call-site variables? Convenient for things like
  `script-dir`.
* Add work recording to scripts.
* Add value dependency tree print to help debug consistency issues.
* Support `collection:map` over map values.
* Change `path:new` to have identity based on source file and occurrence/seed
  value.
* Deduplicate values based on identity.
