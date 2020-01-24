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

## Developer's Corner

### TODO
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
* Improve runtime to be able to get multiple errors with one invocation.
* Improve errors to contain trace-backs of values.
* Consider unifying the CLI so something like:
  ```
  binary filename args...
  ```
  is exactly equivalent to:
  ```
  (load filename) args...
  ```
  in a script.

### Plugin notes
* Ideally, plugins are loaded just like other scripts: `load [path to plugin]`.
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

### Configuration/definition notes
* The program should support two modes of operation (though a clever
  implementation may not need to make these explicit):
  * Basic: Writing and running scripts anywhere.
  * Project: Writing scripts that are self-contained within a project
    directory/hierarchy.
* In project-mode, the program should not look at any files outside of the
  project (so that consistency and repeatability is all within the project
  boundaries).
* In project-mode, there should be some notion of top-level scripts which are
  available to all scripts. Loaded scripts should only have access to the
  environment resulting from loading the top-level scripts (no inheritance of
  the environment from the script loading another script). This is beneficial to
  reduce complexity of analysis when, say, trying to find where a binding is
  defined. It also allows one to execute any file from any subdirectory in a
  consistent way.
  * Or, should scripts always have a clean environment, but be allowed to load
    from the top-level scripts in some convenient/directory-agnostic way?
* In basic-mode, there should be a number of standard user/global directories
  where scripts are loaded from (and/or that serve as a load path when looking
  up a script to load).

#### Possible implementation
Project-mode is detected by the presence of a particular dotfile somewhere in
the parent hierarchy. That dotfile (or maybe folder) is where project top-level
scripts reside. In the "inherit top-level environment" case, maybe it just loads
the dotfile, and the (map) result of loading the dotfile is put in the
environment. In the "load top-level scripts" case, a folder is used as the root
directory from which other things can load scripts.

Since the program is fairly permissive, it's not necessarily easy to enforce
referral to files only within the project folder. It can, however, be a feature
of the `track` command, among other things (maybe a general path-resolution
context feature would be useful here).

In basic-mode (detected by the lack of a project-mode dotfile), a `.config` and an
`/etc` file/folder will be used, maybe in addition to a different dotfile/folder
in all parent directories. Parent directories will have higher priority than
`.config`, and that will have higher priority than `/etc`. The "load all into
environment" case will simply load from lowest to highest priority.