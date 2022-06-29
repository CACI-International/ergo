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

std:import { :Path, :cache, :env, :exec, fs = {:track} } = $std

run_with_path = fn ^:args -> { exec ~env={PATH = env:vars:PATH} ^args; () }

# Get influxdb-cxx
influx = {
  checkout = Path:for <| fn :dir -> {
      exec git clone "https://github.com/awegrzyn/influxdb-cxx.git" $dir
  }
  include = Path:join $checkout include
  builddir = Path:for <| fn :dir -> {
      run_with_path cmake -DCMAKE_BUILD_TYPE=Release -S $checkout -B $dir
  }
  lib = cache {
      run_with_path ~pwd=$builddir make InfluxDB
      Path:join $builddir lib libInfluxDB.so
  }

  { include, lib, libpath = Path:join $builddir lib }
}

# Create test program
test = cache <| Path:for <| fn :out -> {
    run_with_path c++ -std=c++17 -o $out -I influx:include (track main.cpp) influx:lib
}

test-dist = Path:join (std:script:dir ()) influx-test

# Create command interface
commands = {
  dist = fs:copy $test $test-dist
  clean = fs:remove $test-dist
  test = exec ~env={LD_LIBRARY_PATH = influx:libpath} $test
}
## Commands:
## * dist - build and copy outputs
## * clean - clean outputs
## * test - run the test program
fn :cmd -> commands:$cmd
```

If you make the above script executable, then you can run `./script dist`,
`./script clean`, or `./script test`.

## Documentation
All values can have associated docstrings, which can be accessed with the `-d`
argument. This argument implies `-p`, using your system pager to print the
documentation. If `PAGER` is not set in the environment, this defaults to `less
-F`.

Unfortunately, it seems like older Mac versions have a buggy `less` which will
not work correctly with this flag, so the default is `less`. You can:
* set `PAGER` as `more -F` (which on mac is actually `less` in `LESS_IS_MORE` mode, but seems
  to work),
* install a newer version of `less` with brew and set `PAGER` to that, or
* add `-p` to _disable_ paging when displaying the documentation.

## Development Notes

### TODO
* Extend the Ctrl-C behavior to better cancel/stop execution.
  * Right now it only cancels tasks.
* Persist command timing information for better estimates.
* Add value dependency tree print to help debug consistency issues.
* Debugger and profiler.
* Dropping thread for Values (to avoid possibly large stacks).
* Improve cache reads (batch them).
* Figure out how to add backtraces.
* Invalidate and/or remove cache entries.
* Possibly don't evaluate eval_for_id values within Unbound bodies.
  * This would just have fewer surprises, and some things would "just work" like
    using `ergo` or `std:dynamic:eval` within function bodies (as opposed to
    `std:ergo-lazy`, or adding `!no-id`). The price would be that an Unbound's
    identity would be only based on captures (so e.g. it would rely on `std`
    rather than `std:fs:write` if `std:fs:write` appears in the body).
  * Semantically, one could consider `eval_for_id` as acting on the "immediate"
    data tree (where Unbounds need input to produce the value).
  * A bit of work would be needed to still allow Unbound to inherit eval_for_id
    in a logical way.
* Possibly figure out a way for late bindings to propagate through certain
  function calls (e.g. `ergo abc` should "just work" if late bound).
