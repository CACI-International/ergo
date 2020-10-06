# Tracking Inputs

You may have noticed that if we run our script multiple times, it keeps
rebuilding the output even though the inputs haven't changed. This will not do!
Let's change the script so that it will only rerun the command if the source
files change.

```ergo
{{#include example/build_track.ergo}}
```

> You probably noticed that we have used `:` in the `env` directive. When `:`
> is not followed by a value (or when a nested expression is empty, `()`), it
> evaluates to the unit value. In the case of the `env` directive, this indicates
> that the environment variable should be inherited.

We made a few changes in and around our `exec c++` command, and also added a
`fs:copy` command and the `value:cache`, `seq`, `fs:track`, and `path:new` commands:

* `seq` is a function which will make each subsequent value depend on the last.
  That is, it allows you to manually make values depends on other values, which
  changes their identity. The value identity is a hash which represents all
  dependent information of a value.
* `value:cache` is a function which persists the single value argument to disk
  in a cache based on the value identity (hash). Thus, if the identity changes
  from dependencies changing, it will cache a new value. If the identity does
  not change, the cache will find the value and load it _rather_ than evaluating
  the value.
* `path:new` is a function which produces a new value which will be a random
  path. You can use it to create a new path on disk.
* `fs:copy` is a function which will copy a file or directory (recursively) to a
  target file or directory.
* `fs:track` is a function which takes a path and derives the identity from the
  file contents. Thus, if the file contents change, the value changes.

> You may have noticed the use of `^` in the scripts. This is **merge** syntax:
> you can merge maps into block environments, arrays into other arrays, and in
> commands, a map merge indicates non-positional arguments whereas an array
> merge indicates positional arguments. Thus, in the `seq` command, we are
> passing each entry in that array (which effectively is each line within the
> array, since newlines separate entries) as a value to `seq`.

So, in the above binding of `exe` in the script, we:
1. create a new path,
2. run the `exec c++` command with tracked inputs (so its result has an identity
   that changes if the contents of `main.cpp` or `lib.cpp` change) and writing
   to the new path we made,
3. return the new path, making its identity depend on the execution of `c++`
   (since the path is *written/created* by `c++`),
4. cache the returned path, which now depends on `c++` running, which in turn
   depends on the contents of `main.cpp` and `lib.cpp`.

Finally, we copy the (cached) `exe` path to a local, named file.

## Why do we need `path:new`/`fs:copy`?

If we just left the same `-o forty_two` argument to the `c++` command, the
returned future represents successful completion of that command. The `exec`
command which creates this future has no way of knowing that the user is running
the command _for the output file_; there could be any number of other side
effects that the user is interested in. Thus, we need to create an output file
future which `exec` does not try to re-create (by running `c++`) if it already
exists. Thus, the `value:cache` of the output file ensures that we only rerun
`c++` as needed. We could overwrite the same file every time, however with this,
if we were to change one of the input files and then revert the change, our old
cached value would still be around. It is perfectly acceptable to relax this
intense caching if desired. It would be fairly straightforward to cache based
only on, for instance, compiler flags.

These output paths are stored in a local `.ergo_work` directory, so to get a
user-friendly version of the path we can either link or copy it from there. Thus
we use `fs:copy` to expose it, and since this is the last value in the script,
it will cause the `value:cache` function to run, which will either load the
cached value or execute `c++`.
