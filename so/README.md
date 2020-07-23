# so program

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
#!/usr/bin/env so

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
Project-mode is detected by the presence of a `workspace.sos` directory
somewhere in the parent hierarchy. That directory is where project top-level
scripts reside.

In basic-mode (detected by the lack of a project-mode directory), a `so` user
configuration directory is used.

Within a script, the following are defined:
* `work-dir`: this is the directory that contains the currently-executing
  script.
* `load-path`: an array of paths (that can be altered within scripts) that are
  used when resolving `so` calls to load external scripts. In any given script,
  it will by default contain the `work-dir` directory. In basic-mode, it will
  also contain the path to the OS-specific configuration directory:
  * __Linux__: `$XDG_CONFIG_HOME/so` or `$HOME/.config/so`
  * __macOS__: `$HOME/Library/Preferences/so`
  * __Windows__: `{FOLDERID_RoamingAppData}\so\config`

## Development Notes

### TODO
* Data-manipulation functions (maybe as a plugin).
* Improve errors to contain trace-backs of values.
  * Find specific examples where this is useful.
* Self-documentation. Accessed and printed directly while evaluating scripts
  and/or accessed with a `--doc` command-line argument. Add a way to attach
  documentation (or other metadata?) to arbitrary types in scripts.
* Allow access to logging from scripts. Allow access to stdin/stdout/stderr.
  Allow mutually exclusive use of stdin/stdout/stderr for interactive programs.
* Allow explicit error signaling from scripts. Maybe allow catching errors?
* Add 'force' to force a value to be immediately evaluated?
* Add error list at bottom of tty status output, and support continuing when an error occurs.
  * Display more detailed error information at exit?
* Add a message/signal handler to exit gracefully.
* Revisit pattern literal matching.
  * This could use some sort of equality test on the value _data_, rather
    than comparing value identifiers.
* Persist command timing information for better estimates.
* Block set shorthand (if there's a line in a block that is just a string,
  insert the environment value equal to that string into the block environment).
* `string words`, `string lines`, `fetch`, maybe `fs mount` to open tarballs,
  zips, directories, urls to such?
* Allow setting map keys from values rather than string constants?
* Extend map, fold to operate on maps as well?
* Reduce function captures to the bare minimum, and allow functions to be called
  in a delayed context.
* Allow functions to get call-site variables? Convenient for things like
  `work-dir`.
* Add union types to the type system, allowing `if` and `match` to be delayed.
* Allow fetching of urls and use of zip/tarballs with load command.

### Plugin notes
* Ideally, plugins are loaded just like other scripts: `so [path to plugin]`.
  Duplication is handled as one would expect (only load things once). The loaded
  plugin must return a Value (just like loading another script).
* When loaded, plugins may also provide extensions to share interfaces/memory
  with other plugins or the runtime. This may include hooks into certain events
  (execution complete, loading, etc). Extensions may rely on other extensions.
  * If extension dependency resolution is necessary, there either needs to be a
    single point at which resolution occurs (maybe after loading certain prelude
    files) or extensions must be loaded in a particular order (though this
    precludes circular dependency resolution).
* ABI must be stable at plugin boundaries.

#### Native vs WASM plugins
* Both types of plugins could be made in other languages if desired (though not
  as convenient as types would have to be redefined to be ABI-compatible).
  With regard to Rust:
  * Native plugins could be smaller, but would need to be built for separate
    platforms.
  * WASM code by default is kind of large: a hello world comes out over a MB.
    With std disabled it gets much smaller. This may be appropriate anyway since
    std is not ABI-stable. Size may not be a big concern since they won't be
    downloaded often. WASM allows the plugins to be loaded on any platform.
