# Concurrency

Anyone familiar with C/C++ build tools know that we are missing out on some
valuable concurrency in our script! For this simple program there's really not
much to be gained here, but for more complex code &mdash; especially C++ code
&mdash; this can make a huge difference. Let's change the script to perform
compilation and linking separately:

```sh
{{#include example/build_concurrent}}
```

From what we've covered so far, it should be clear how these changes work to
achieve concurrent compilation.

However, now we're getting into duplication territory. It'd be nice if we could
make the compile step a repeatable block. It'd also be nice to use a list of
files as our input data.

## Enter the function
Bob scripts support creating functions. The functions actually behave more like
macros in other languages; they are late-bound and any environment lookups occur
at the call site.

Functions are introduced with the `fn` keyword. Following the keyword should be
exactly one argument, which serves as the body of the function. Often this
argument will be a map/block (`{...}`). Arguments to the function will be bound
as an array to `@` in the environment when the function body is evaluated.
There's nothing particularly special about `@`; it can be used as a binding name
like any other, but it happens to be the one used in function evaluation as
well.

Let's make compiling and linking into functions:

```sh
{{#include example/build_concurrent_fn}}
```

This is looking much more manageable. Note that both functions have a block as
their expression, and the final expression of the block is what is returned when
the function is invoked. Also note that those final values are evaluating to
bindings, since nesting expressions with `$...` or `(...)` is only necessary for
any arguments to a command. If we were to change `exe` to `$exe` in the
`link_exe` function, that would instead run the executable!

## Mapping over arrays
Ideally we'd like to map the `compile` function over an array of the files;
luckily there's a built-in command for that, `map`. It takes a function and an
array and maps the function on each value in the array, returning an array with
the result.

Let's take advantage of this:
```sh
{{#include example/build_concurrent_map}}
```

> As previously mentioned, to make a data array rather than one which evaluates
> each value as a command, we precede the `files` array with `$`.

We've also condensed our final command by nesting commands in it.

## Command correctness and reproducibility

If you were running the script file as these changes were made, you may have
noticed that only `ln` was run each time (as it always is when outputs are
readily available). The three versions of the script on this page are
functionally identical, so even though they look fairly different, they share
the same outputs.
