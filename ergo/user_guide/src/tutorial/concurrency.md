# Concurrency

Anyone familiar with C/C++ build tools know that we are missing out on some
valuable concurrency in our script! For this simple program there's really not
much to be gained here, but for more complex code &mdash; especially C++ code
&mdash; this can make a huge difference. Let's change the script to perform
compilation and linking separately:

```sh
{{#include example/build_concurrent.ergo}}
```

From what we've covered so far, it should be clear how these changes work to
achieve concurrent compilation. Remember, the `exec` commands (and other
commands) aren't doing their actions immediately, but rather returning future
values which are only forced as a chain of dependencies by the final value of
the script.

However, now we've got a lot of duplication. It'd be nice if we could make the
compile step a repeatable block. It'd also be nice to use a list of files as our
input data.

## Functions
`ergo` scripts support creating functions. User functions, like builtin functions,
are evaluated immediately at the call site, but only have access to the
environment scope that was present at their definition.

Functions are introduced with the `fn` keyword. Following the keyword should be
a **pattern**, indicating the expected positional and non-positional arguments.
Then, an arrow `->` followed by the body of the function is expected. Often this
body will be a map/block (`{...}`), but it can be any expression. When called,
the function argument pattern will be checked against the arguments and, if it
matches, any bindings in the pattern will be present in the execution of the
body.

Let's make compiling and linking into functions:

```sh
{{#include example/build_concurrent_fn.ergo}}
```

This is looking much more manageable. Note that both functions have a `cache
seq` expression, and the final expression of the block is what is returned when
the function is invoked.

## Mapping over arrays
Ideally we'd like to map the `compile` function over an array of the files;
luckily there's a built-in command for that, `map`. It takes a function and an
array and maps the function on each value in the array, returning an array with
the result.

Let's take advantage of this:
```sh
{{#include example/build_concurrent_map.ergo}}
```

We've also condensed our final command by nesting commands in it. Note the `^`
array merge to pass the resulting array values of the `map` function each as
arguments to `link_exe`.

## Command consistency and reproducibility

If you were running the script file as these changes were made, you may have
noticed that only `fs copy` was run each time (as it always is when outputs are
readily available). The three versions of the script on this page are
functionally identical, so even though they look fairly different, they share
the same outputs. The value identities are the same throughout.
