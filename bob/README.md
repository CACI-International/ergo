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
