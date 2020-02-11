# Tracking Inputs

You may have noticed that if we run our script multiple times, it keeps
rebuilding the output even though the inputs haven't changed. This will not do!
Let's change the script so that it will only rerun the command if the source
files change.

```sh
{{#include example/build_track}}
```

We made three changes in our `c++` command, and also added a `ln` (link)
command. We've also introduced some new syntax: parentheses and the dollar sign.
Parentheses embed an expression (in this case two `track` commands), and the
dollar sign is shorthand for embedding with parentheses: `$exe` is the same as
`(exe)` (thus it is evaluating to the binding for `exe` in the environment).
When a string follows the dollar sign, everything until the first whitespace is
used as the expression to evaluate, but you can also use `${...}` and `$[...]`
to evalute maps and arrays in command-position. This changes the default
interpretation of array values and values set in the map to be as commands
rather than arguments (i.e., command evaluation vs. data). Don't worry if this
isn't immediately clear; there will be examples of this difference presented
later.

> You probably noticed that we have already used `$` in the `env` directive.
> When `$` is not followed by a value (or when a nested expression is empty,
> `()`), it evaluates to the unit value. In the case of the `env` directive,
> this indicates that the environment variable should be inherited.

In the `c++` command, we first changed the output file to a special directive,
`file` (just like `env` is a directive). The `file` directive is a feature of
the `exec` command. It creates a unique path that will be passed in its position
as an argument to the command (in this case, passing a unique file name to `c++
-o`), and binds that path to the given name in the environment (in this case,
`exe`).

We also changed the two input files by passing them to the `track` command. The
`track` command is a built-in command which takes a path and returns a file path
future which tracks when the file changes using a hash of the file's contents.

Finally, we added a `ln` command to link the output file (stored in `exe`) to
the desired output file in the current directory, `forty_two`.

## Why do we need `ln`?

If we just left the same `-o forty_two` argument to the `c++` command, the
returned future represents successful completion of that command. The `exec`
command which creates this future has no way of knowing that the user is running
the command _for the output file_; there could be any number of other side
effects that the user is interested in. Thus, we need to create an output file
future which `exec` does not try to re-create (by running `c++`) if it already
exists. The output file path is generated based on other arguments to the
command, so if the input files change, the output file path will be different
and `c++` will be run to make sure it exists.

These output paths are stored in a local `.so_work` directory, so to get a
user-friendly version of the path we can either link or copy it from there.

Since the `ln` command uses `$exe` (which evaluates to the output file) and is
the final value of the script, the `ln` command future is evaluated, and it in
turn will evaluate the `exe` future to get the output path.
