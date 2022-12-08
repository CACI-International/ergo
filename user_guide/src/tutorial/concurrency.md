# Concurrency

Anyone familiar with C/C++ build tools know that we are missing out on some
valuable concurrency in our script! For this simple program there's really not
much to be gained here, but for more complex code &mdash; especially C++ code
&mdash; this can make a huge difference. Let's change the script to perform
compilation and linking separately:

```ergo
{{#include example/build_concurrent.ergo}}
```

Here we've changed a few things:
* `task` is our concurrency primitive; it will evaluate its argument
  concurrently with other tasks,
* the `exec` call is put in a block followed by a `()` (unit); this causes
  `exec` to be fully evaluated while the task is run. If we didn't have the
  `()`, the value of `exec` would be returned immediately (as the exec'd value
  runs asynchronously) so the task would not encapsulate the action we want.
  This is the value of using blocks (`{..}`) to sequence commands.

From what we've covered so far, it's hopefully clear how the rest of the changes
to our script achieve concurrent compilation. Remember, the script expressions
aren't evaluated immediately, but rather are only evaluated if needed when
evaluating the value returned by the script (in this case, the last line of the
script).

However, now we've got a lot of duplication. It'd be nice if we could make the
compile step a repeatable block. It'd also be nice to use a list of files as an
input.

## Functions
`ergo` scripts support creating functions. User functions behave just like
standard library functions (and indeed some standard library functions are
implemented in scripts).

Functions are created with `fn ... -> ...`. Everything following `fn` indicates
the expected positional and keyed arguments. For example `:value` indicates the
binding to _set_ in the environment when the function is called. The arrow
(`->`) separates the function arguments from the body. Often this body will be a
map/block (`{...}`), but it can be any expression. When called, the function
arguments will be bound with the call site arguments, and if no errors occur
from this, the body expression is returned (capturing the arguments).

Let's make compiling and linking into functions:

```ergo
{{#include example/build_concurrent_fn.ergo}}
```

This is looking much more manageable. Note that both functions have a `cache`
call, and the final value of the block (just that returned by `task`) is what is
returned when the function is invoked.

> You may have noticed the use of the `^` operator. This is a _merge_ operator.
> In the `fn`, it indicates that the rest of the value (in this case, the
> arguments) should be bound to that expression. In the function body, it merges
> the collected values into the `exec` command. So here, `^$objs` takes all
> remaining arguments to the `link-exe` function and then passes them as
> arguments to `exec`.

We have added comments starting with `## ` before the functions. These are _doc
comments_. You can add documentation to _any_ value (not just functions), and
the documentation can be retrieved with the builtin `doc` function (and the
command-line `-d`/`--doc` argument). Note that the space after `##` is required,
otherwise `##` is used to apply attributes to values (also not covered here).

We've also used a feature for the first time: composite strings. The strings
passed to `task` have a dollar operator (`$`) in them. This will use the
following word (where a word is characters `-_A-Za-z0-9`) as a binding in scope,
or grouped expression (parens, curly brackets, etc), and display it in the
string. For example, `$source-file` refers to a `source-file` binding, and
`$(cmd a b c)` would call `cmd a b c` and display the result in the string. You
can use the dollar operators in quoted strings, block strings, and doc comments.

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
