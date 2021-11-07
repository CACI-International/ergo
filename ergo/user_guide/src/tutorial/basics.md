# Basics

To get the ball rolling, let's assume we have a directory with the following
layout:

```sh
project_root
 - lib.cpp
 - lib.h
 - main.cpp
```

A real project would likely have a more organized (and complex) layout, however
it doesn't make much of a difference for the purposes of this tutorial.

For the sake of a functional (albeit boring) demo, let's also define our C++ files.

`lib.cpp`:
```c++
{{#include example/lib.cpp}}
```

`lib.h`:
```c++
{{#include example/lib.h}}
```

`main.cpp`:
```c++
{{#include example/main.cpp}}
```

## Our first script
Now that we have a directory with C++ source files, we probably want to compile
them! At the moment, we just want to create a final executable, although later
we'll also make a shared library.

Let's create a new ergo script file, called `build.ergo` (though you can call it
whatever you want, with or without an extension):
```ergo
{{#include example/build_basic.ergo}}
```

Simple enough, right? Let's break down exactly what's happening here:

__Line 1__: The shebang at the top is optional, but with it we can make this
script file executable and run it directly. Like any shebang, it could just be
the direct path to the `ergo` executable, but using `/usr/bin/env` is encouraged.

__Line 3__: A comment! Just like many other languages, `#` begins comments. The
commented portion will continue until the end of the line.

__Line 4__: A command to compile `main.cpp` and `lib.cpp` into an executable
named `forty_two`.

If you run this script, you will see `forty_two` is now in the current
directory. Right now, this script is barely different than a shell script (both
in function and in parts of the syntax).

### Closer examination
While our script is currently functionally identical to a shell script, under
the hood the representation of our script is much different. When line 4 is
executed in the ergo runtime, it *is not* immediately executing `c++`.

Instead,

1. The entire expression is evaluated, producing a lazily-evaluated value which
   will later be evaluated. The value captures `std:exec` and `std:env:get` to
   be used later, which are functions in the standard library.
2. The value returned by the last line of our script is then _evaluated_, which
   is what actually invokes `exec`, running the `c++` program with the given
   arguments.
3. The `exec` function is applied to the arguments (where `(env = ...)` is
   setting a keyed argument that `exec` uses for environment variables). `exec`
   passes `c++` to the system's `exec` system call, which will look it up in a
   few fixed places. It then returns a value of the result of running the
   program. This omits some detail; the returned value actually contains
   multiple values representing things like stdout, exit code, etc, and worth
   noting is that the program is run asynchronously.
4. The value returned from `exec` is displayed on the command-line.

Thus, `c++` is run only because the value of the last line of the script will
run it.

The `(env = ...)` is indicating to the `exec` command that the environment
should contain the same `PATH` variable as the `ergo` process. By default,
`exec` has an empty environment to make environment usage explicit. Most `c++`
binaries (`gcc`- or `clang`-based, for example) use the `PATH` to find the
system linker (`ld`), so the easiest way to make this work is to forward the
script `PATH` (though you could also manually specify `PATH=/usr/bin`).

## Syntax and execution
We'll see more syntax examples in the coming chapters, but as a quick primer to
get a grasp of what is supported, we'll shortly discuss syntax here.

### Most things are strings
Like shell languages, bare text is interpreted as a string. Strings can contain
any character except special characters used in the rest of the syntax and
whitespace. However, you can create strings containing arbitrary characters by
surrounding them with double quotes (supporting escape sequences) or single
quotes (for raw strings).

```ergo
this_is_a_string
"this is a quoted {} []: \"string\"\n"
these are each individual strings
'this is a raw string'
''this raw string contains a ' character''
```

### Compound data
Arrays and maps are also supported in scripts. Items in arrays and maps can be
separated by commas, newlines, or semicolons indiscriminately.

```ergo
[this,is,an,array]
[newlines
 separate
 items
 too]
{ key = value, otherkey = othervalue }
```

The syntax for maps is actually the **same** as sequential code blocks; they
open a new environment scope (which can have bindings that shadow outer
environments), and the block itself evaluates to the map of environment bindings
if the final expression in the map is a binding:

```ergo
{
  file = main.cpp
  output = std:exec c++ $file
}
```

Evaluates to a map with `file` and `output` as keys, whereas

```ergo
{
  file = main.cpp
  std:exec c++ :file
}
```

evaluates to the resulting value(s) from the `exec` command.

Note that the bindings are done in top-to-bottom order, so later bindings can
use earlier ones (unlike basic maps in other languages where the key-value
bindings are disparate).

### Commands
Commands take positional and keyed arguments as inputs (as seen in `(env=...)`)
and can interpret them in arbitrary ways. For instance, `exec` accepts a number
of keyed arguments (as we saw with `env`) and tries to convert positional
arguments into strings suitable for passing to an external program.

You may nest commands with parentheses:
```ergo
command1 arg1 (command2-giving-arg2 a b c) arg3
```

The first value of a command, if it is a string literal, is queried in the
lexical scope to resolve to a bound value.

Commands are identified by the presence of arguments. To call a command without
arguments, add a `:` after the value to call (e.g. `command:`).

### Binding Retrieval
Getting any value from the binding scope is done by using a colon. For instance,
`:something` will retrieve the value bound to `something` in the enclosing
scope. Similarly, indexing into a map or array also uses a colon, except prior
to the colon the value to index is provided. Like commands, if the value to
index is a string, it will be retrieved from the current bindings. Thus,
`my_map:my_key:key2` and `:my_map:my_key:key2` are the same (the leading colon
is unnecessary).
