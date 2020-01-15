# bob program

Current features include:

* A hybrid-typed scripting language that (in many ways) closely resembles 
shell scripts.
* Automatic concurrent processing of commands.
* Dependency tracking for all produced values.

Planned features include:
* A platform-agnostic plugin system based on WASM.
* Persistent database of command statistics aggregated over multiple runs and
used to estimate future performance.

## Script Example

```sh
#!/usr/bin/env bob1

use_path = { env = { PATH = $ } }

# Get influxdb-cxx
influx = {
  git clone "https://github.com/awegrzyn/influxdb-cxx.git" { dir = checkout }
  influx-include = checkout include .
  cmake -DCMAKE_BUILD_TYPE=Release -S (checkout .) -B { dir = builddir } $use_path
  make InfluxDB $use_path ${
    pwd = builddir .
    creates = {
      influx-lib = builddir lib libInfluxDB.so .
    }
  }

  {
    include = checkout include .
    lib = influx-lib
    libpath = builddir lib .
  }
}

# Compile main
c++ -std=c++17 -c -o { file = main } -I $influx:include (track main.cpp)
# Link program
c++ -o { file = test } $main $influx:lib $use_path

# Create output map
{
  dist = ln -f $test influx-test
  clean = rm -f influx-test
  * = $test { env = { LD_LIBRARY_PATH = $influx:libpath } }
}
```

## TODO
* Consider whether index notation is necessary at all, or whether it's more
  uniform to use command notation for indexing (some shorthand notation may
  still be useful on the command-line).
* Data-manipulation functions (maybe as a plugin).
* Consider making arrays and maps always argument-position by default, and only
  command-position when in a nested expression.
* Consider requiring `exec` explicitly, rather than falling back to it. This
  allows certain errors to be obvious immediately (otherwise, for instance, a
  command that is mistyped will be "hidden" behind the future returned by exec,
  and will only become apparent at future execution time). This could also be
  changed with a setting, or by rebinding and clearing exec in the environment.
* Better function syntax, to make `map` less ugly?
* Change 'once' to be a command itself rather than a key on exec results (it can
  be generalized to arbitrary values). Add 'cache' command to execute once and
  store result (or merge the two).
