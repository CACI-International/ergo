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
* `Path:with-output` is a convenience function that captures the typical use
  case of returning an output path that is created by a _function_. So we pass a
  function (described in more detail in the next section) which takes a single
  argument and returns the value which writes that argument (which will be a new
  path). This replaces the `{out = Path:new (), ..., :out}` pattern in the
  previous version of our script.
* `task` is our concurrency primitive; it will evaluate the argument
  concurrently.

From what we've covered so far, it should be clear how the rest of the changes
to our script achieve concurrent compilation. Remember, the script expressions
aren't evaluated immediately, but rather are unevaluated values which are only
evaluated if needed when evaluating the value returned by the script (in this
case, the last line of the script).

However, now we've got a lot of duplication. It'd be nice if we could make the
compile step a repeatable block. It'd also be nice to use a list of files as an
input.

## Functions
`ergo` scripts support creating functions. User functions behave just like
standard library functions (and indeed some standard library functions are
implemented in scripts).

Functions are created with `fn`. Following the keyword should be a **pattern**
expression, indicating the expected positional and keyed arguments. Such
expressions are similar to normal expressions, but with a few exceptional
parsing differences. For instance `:value` indicates the binding to _set_ in the
environment rather than _get_. An arrow `->` separates this pattern expression
(the arguments of the function) from the body. Often this body will be a
map/block (`{...}`), but it can be any expression. When called, the function
arguments from the pattern expression will be matched against the call site
arguments, and if no errors occur from this, the body expression is returned
(capturing the arguments).

Let's make compiling and linking into functions:

```ergo
{{#include example/build_concurrent_fn.ergo}}
```

This is looking much more manageable. Note that both functions have a `cache`
call, and the final value of the block (just that returned by `task`) is what is
returned when the function is invoked.

> You may have noticed the use of the `^` operator. This is a _merge_ operator.
> In a pattern expression (like our `link-exe` fn pattern), it indicates that
> the rest of the value (in this case, the arguments) should be bound to that
> expression. In a normal expression, it merges the value into the enclosing
> command, block/map, or array. So here, `^:objs` takes all remaining arguments
> to the function and then passes them as arguments to `exec`.

We have added comments starting with `## ` before the functions. These are _doc
comments_. You can add documentation to _any_ value (not just functions), and
the documentation can be retrieved with the builtin `doc` function (and the
command-line `-d`/`--doc` argument). Doc comments can also have nested
expressions that will be evaluated to generate the content, but we won't cover
that here. Note that the space after `##` is required, otherwise `##` is used to
apply attributes to values (also not covered here).

We've also used a feature for the first time: composite strings. The strings
passed to `task` have a merge operator (`^`) in them. This will use the
following word (where a word is characters `-_A-Za-z0-9`) as a binding in scope,
or grouped expression (parens, curly brackets, etc), and display it in the
string. For example, `^source-file` refers to `:source-file`, and `^(Path:new
())` would insert a random path. You can use the merge operator in quoted
strings, block strings, and doc comments.

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
