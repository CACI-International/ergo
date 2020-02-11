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
this doesn't make much of a difference for the purposes of this tutorial.

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

Let's create a new so script file, called `build` (though you can call it
whatever you want, with or without an extension):
```sh
{{#include example/build_basic}}
```

Simple enough, right? Let's break down exactly what's happening here:

__Line 1__: The shebang at the top is optional, but with it we can make this
script file executable and run it directly. Like any shebang, it could just be
the direct path to the `so` executable, but using `/usr/bin/env` is encouraged.

__Line 3__: A comment! Just like many other languages, `#` begins comments. The
commented portion will continue until the end of the line.

__Line 4__: A command to compile `main.cpp` and `lib.cpp` into an executable
named `forty_two`.

If you run this script, you will see output indicating that `c++` is invoked.
And sure enough, `forty_two` is now in the current directory. Right now, this
script is barely different than a shell script (both in function and most of the
syntax, so you could invoke `sh build` if you wanted after commenting out the
`{...}` at the end of the line).

### Closer examination
While our script is currently functionally identical to a shell script, under
the hood the representation of our script is much different. When line 4 is
executed in the so runtime, it *is not* immediately executing `c++`.

Instead,

1. The so runtime tries to find `c++` in the environment bindings.
2. When it does not find `c++`, it treats the line as if it were written as
   `exec c++ ...`. That is, it instead uses `exec` from the environment bindings
   (this is built in) as the command to run.
3. The `exec` command looks up `c++` using the `PATH` environment variable, just
   like a shell would do. It then creates a future representing the execution of
   the `c++` program. This is omitting some detail: the returned data actually
   contains multiple futures representing things like stdout, exit code, etc.

Since the last line of our script evaluates to a future, that future is then run
to completion, which is what invokes the `c++` program with the given arguments.

The `{ env = ... }` is indicating to the `exec` command that the environment
should contain the same `PATH` variable as the parent script. By default, _all_
commands have an empty environment to foster reproducibility. Most `c++`
binaries (`gcc`- or `clang`-based, for example) use the `PATH` to find the
system linker (`ld`), so the easiest way to make this work is to forward the
script `PATH` (though you could also manually specify `PATH=/usr/bin`).

## Syntax and execution
We'll see more syntax examples in the coming chapters, but as a quick primer to
get a grasp of what data is supported, we'll shortly discuss syntax and data
here.

### Strings have priority
Like shell languages, bare text is interpreted as a string. Strings can contain
any character except special characters used in the rest of the syntax and
whitespace. However, you can create strings containing arbitrary characters by
surrounding them with quotes.

```sh
this_is_a_string!
"this is a quoted {} []: \"string\"\n"
these are each individual strings
```

### Compound data
Arrays and maps are also supported in scripts. Items in arrays and maps can be
separated by commas, newlines, or semicolons indescriminately.

```sh
[this,is,an,array]
[newlines
 separate
 items
 too]
{ key = value, otherkey = othervalue }
```

Maps are actually the same as nested code blocks; they open a new environment
scope (which can have bindings that shadow outer environments), and the block
itself evaluates to the map of environment bindings *if* the final expression in
the map evaluates to the unit type (all bindings do):

```sh
{
  file = main.cpp
  output = c++ $file
}
```

This is valid since these are not merely disparate key-value pairs, but instead
top-to-bottom evaluated bindings in an environment.

### Commands
Commands are presented with a list of values and can interpret them in arbitrary
ways. For instance, the `exec` command interprets arrays merely as if each item
were a separate argument, maps as directives to more complex functionality (as
we saw with `env`), and strings as literal arguments to the command.

The first value of a command (determining which command to invoke, if any) is
determined as follows:

* If the value is a string, the current environment is queried to determine
   whether a binding for that string exists.
  * If a binding exists:
    * if it is a function and arguments were provided, the function is applied to
      the remaining values,
    * otherwise, the value is returned (effectively evaluating the binding).
  * Otherwise, the `exec` command is passed all of the arguments.
* If the value is a file path future, the `exec` command is passed all of the
  arguments.
* If the value is a unit-value, the next argument is evaluated and the result is
  returned (useful for returning a plain string from a command, among other things).
* Otherwise (a fairly uncommon case), the value is returned as-is without doing anything with the
  arguments.
