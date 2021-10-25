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
* Deduplicate values based on identity.
* Strongly control the lifetime of the thread pool using weak references in the
  runtime.
  * This will allow loaded plugins to be properly dropped rather than leaked.

* Add cache slots.
* Rework match error behavior.
* Improve sources of capture bindings.
* Debugger and profiler.
* Script mutexes?
* Improve UI error behavior.
  * Display the aggregated errors at the end of execution (rather than the
    returned error value).
* `Iter:sort`/`Iter:order`, `Iter:count`, and numeric comparison functions
* Automatically cache dynamic value access.
* Tree comments.
* Change how the default primitive value documentation is applied (either remove
  it entirely or do not set it as metadata and just change the documentation
  lookup function).
* Improve error behavior of reqwest::blocking::Client::new().
* Improve `std:Path:or-source` behavior when a non-Path is passed.
* Add deep merging.
* Add script-specified network cache locations (for local vendoring).
* Add hostname-based(?) request parameters.
