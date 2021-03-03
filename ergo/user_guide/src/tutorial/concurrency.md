# Concurrency

Anyone familiar with C/C++ build tools know that we are missing out on some
valuable concurrency in our script! For this simple program there's really not
much to be gained here, but for more complex code &mdash; especially C++ code
&mdash; this can make a huge difference. Let's change the script to perform
compilation and linking separately:

```ergo
{{#include example/build_concurrent.ergo}}
```

Here we've changed a few things to take advantage of a few standard library
functions:
* `std:import` is used to specifically import particular indices of a value into
  the current scope, so for instance here we are taking `cache`, `env`, etc.
  from the standard library (rather than dumping the entire standard library
  into the top-level scope).
* `Path:with-output` is a convenience function that captures the typical use
  case of creating an output path and having it depend on the command which
  creates it. So we pass a _function_ (described in more detail in the next
  section) which takes a single argument and returns the value which writes that
  argument (which will be a new path). This replaces the `seq ^[out = Path:new:,
  ..., :out]` pattern in the previous version of our script.
* `task` is our concurrency primitive; it will return a value which, when
  evaluated, will evaluate its value argument concurrently.

From what we've covered so far, it should be clear how the rest of the changes
to our script achieve concurrent compilation. Remember, the `exec` commands (and
other commands) aren't doing their actions immediately, but rather returning
values which are only evaluated as a chain of dependencies by the final value of
the script.

However, now we've got a lot of duplication. It'd be nice if we could make the
compile step a repeatable block. It'd also be nice to use a list of files as our
input data.

## Functions
`ergo` scripts support creating functions. User functions, like builtin functions,
are evaluated immediately at the call site, but only have access to the
environment scope that was present at their definition.

Functions are introduced with `fn`. Following the keyword should be a
**pattern** expression, indicating the expected positional and non-positional
arguments. Such expressions are similar to normal expressions, but for instance
`:value` indicates the binding to _set_ in the environment rather _get_. Then,
an arrow `->` followed by the body of the function is expected. Often this body
will be a map/block (`{...}`), but it can be any expression. When called, the
function argument pattern will be checked against the arguments and, if it
matches, the body will be executed with the bindings from the pattern set.

Let's make compiling and linking into functions:

```ergo
{{#include example/build_concurrent_fn.ergo}}
```

This is looking much more manageable. Note that both functions have a `cache`
call, and the final value of the block (just that returned by `task`) is what is
returned when the function is invoked.

## Mapping over arrays
Ideally we'd like to map the `compile` function over an array of the files;
luckily there's a standard library function for that, `Iter:map`. It takes a
function and a value that can be converted to an iterator (like an array) and
maps the function on each value, returning an iterator of the results.

Let's take advantage of this:
```ergo
{{#include example/build_concurrent_map.ergo}}
```

We've also condensed our final command by nesting commands in it. Note the `^`
array merge to pass the resulting values in `objects` as individual arguments to
`link_exe`.

## Command consistency and reproducibility

If you were running the script file as these changes were made, you may have
noticed that only `fs:copy` was run each time (as it always is when outputs are
readily available). The three versions of the script on this page are
functionally identical, so even though they look fairly different, they share
the same outputs. The value identities are the same throughout.
