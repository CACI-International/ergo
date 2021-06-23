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
* Add a convenience function to turn a map into command-line flags (based on a
  specification of which flags have required args). This is convenient for
  things that want to pass flags to a program but also might need to easily
  inspect/manipulate the flags.
