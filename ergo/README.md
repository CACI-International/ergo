# ergo program

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
#!/usr/bin/env ergo

run_with_path = fn ^{^kwargs} ^args -> (exec ^$kwargs ^{env = {PATH = $}} ^$args) complete

# Get influxdb-cxx
influx = {
  checkout = seq ^[
      dir = path new
      exec git clone https://github.com/awegrzyn/influxdb-cxx.git $dir
      $dir
  ]
  include = (path join $checkout include)
  builddir = seq ^[
      dir = path new
      run_with_path cmake -DCMAKE_BUILD_TYPE=Release -S $checkout -B $dir
      $dir
  ]
  lib = cache seq ^[
      run_with_path ^{ pwd = $builddir } make InfluxDB
      path join $builddir lib libInfluxDB.so
  ]

  { include, lib, libpath = path join $builddir lib }
}

# Create test program
test = cache seq ^[
    out = path new
    run_with_path c++ -std=c++17 -o $out -I (influx include) (track main.cpp) (influx lib)
    $out
]

test-dist = path join $work-dir influx-test

# Create output map
{
  dist = fs copy $test $test-dist
  clean = exec rm -f $test-dist
  test = exec ^{env = {LD_LIBRARY_PATH = influx libpath}} $test
}
```

## Modes of operation
Project-mode is detected by the presence of a `workspace.ergo` directory
somewhere in the parent hierarchy. That directory is where project top-level
scripts reside.

In basic-mode (detected by the lack of a project-mode directory), a `ergo` user
configuration directory is used.

Within a script, the following are defined:
* `work-dir`: this is the directory that contains the currently-executing
  script.
* `load-path`: an array of paths (that can be altered within scripts) that are
  used when resolving `ergo` calls to load external scripts. In any given script,
  it will by default contain the `work-dir` directory. In basic-mode, it will
  also contain the path to the OS-specific configuration directory:
  * __Linux__: `$XDG_CONFIG_HOME/ergo` or `$HOME/.config/ergo`
  * __macOS__: `$HOME/Library/Preferences/ergo`
  * __Windows__: `{FOLDERID_RoamingAppData}\ergo\config`

## Development Notes

### TODO
* Self-documentation. Accessed and printed directly while evaluating scripts
  and/or accessed with a `--doc` command-line argument. Add a way to attach
  documentation (or other metadata?) to arbitrary types in scripts.
* Allow mutually exclusive use of stdin/stdout/stderr for interactive programs.
* Display more detailed error information at exit?
* Add a message/signal handler to exit gracefully.
  * This has been added but child programs still capture the signal, need to
    change the process group to prevent this.
* Revisit pattern literal matching.
  * This could use some sort of equality test on the value _data_, rather
    than comparing value identifiers.
* Persist command timing information for better estimates.
* Allow setting map keys from values rather than string constants?
* Reduce function captures to the bare minimum.
* Allow functions to get call-site variables? Convenient for things like
  `work-dir`.
* Add work recording to script.
* Either deconstruction with `match`.
* Add value dependency tree print to help debug consistency issues.
* Support using ancestor `workspace.ergo` as root of the storage directory.
