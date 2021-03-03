# Tracking Inputs

You may have noticed that if we run our script multiple times, it keeps
rebuilding the output even though the inputs haven't changed. This will not do!
Let's change the script so that it will only rerun the command if the source
files change.

```ergo
{{#include example/build_track.ergo}}
```

We made a few changes in and around our `exec c++` command, and also added a
number of other commands from the standard library:

* The `^std:` at the top _merges_ the keys of the standard library into the
  top-level environment. Specifically, `std:` returns the standard library and
  `^` merges it. This makes it convenient to use standard library functions
  without always preceding them with `std:`.
* `cache` is a function which persists its argument to disk in a cache based on
  the value identity (hash). Thus, if the identity changes from dependencies
  changing, it will cache a new value. If the identity does not change, the
  cache will find the value and load it _rather_ than evaluating the value.
* `seq` is a function which will make each subsequent value depend on the last.
  That is, it allows you to manually make values depend on other values, which
  changes their identity.
* `Path:new` is a function which produces a new value which will be a random
  path. You can use it to create a new path on disk.
* `fs:track` is a function which takes a path and derives the identity from the
  file contents. Thus, if the file contents change, the value changes.
* `fs:copy` is a function which will copy a file or directory (recursively) to a
  target file or directory.
* `script:dir` is a function that returns the directory containing the
  currently-executing script.

> You may have noticed the use of `^` in the `seq command`. This is a similar
> operation to `^std:`, a **merge** expression: you can merge maps into
> blocks/maps, arrays into other arrays, and in commands, a map merge indicates
> non-positional arguments whereas an array merge indicates positional
> arguments. Thus, in the `seq` command, we are passing each entry in that array
> (which effectively is each line within the array, since newlines separate
> entries) as a value to `seq`.

> You probably also noticed the `<|` and `|>` operators. These are _pipe_
> operators, which are syntax sugar to group code. `<|` will group the
> right-hand side, so for instance `a b <| c d` is the same as `a b (c d)`.
> `|>` groups the left-hand side and `|` groups the left-hand side and moves it
> to be the last argument of the right-hand side.

So, in the above binding of `exe` in the script, we:
1. create a new path,
2. run the `exec c++` command with tracked inputs (so its result has an identity
   that changes if the contents of `main.cpp` or `lib.cpp` change) and writing
   to the new path we made, accessing `complete` of the returned map from `exec`
   to wait for the command to complete,
3. return the new path, making its identity depend on the execution of `c++`
   with `seq` (since the path is *written/created* by `c++`),
4. cache the returned path, which now depends on `c++` running, which in turn
   depends on the contents of `main.cpp` and `lib.cpp`.

Finally, we copy the (cached) `exe` path to a local, named file.

## Why do we need `Path:new`/`fs:copy`?

If we just left the same `-o forty_two` argument to the `c++` command, the
returned value represents successful completion of that command. The `exec`
command which creates this value has no way of knowing that the user is running
the command _for the output file_; there could be any number of other side
effects that the user is interested in. Thus, we need to create an output file
value which `exec` does not try to re-create (by running `c++`) if it already
exists. Thus, the `cache` of the output file ensures that we only rerun `c++` as
needed. We could overwrite the same file every time, however with this, if we
were to change one of the input files and then revert the change, our old cached
value would still be around. It is perfectly acceptable to relax this intense
caching if desired. It would be fairly straightforward to cache based only on,
for instance, compiler flags.

The generated paths are stored in a local `.ergo_work` directory, so to get a
user-friendly version of the path we can either link or copy it from there. Thus
we use `fs:copy` to expose it, and since this is the last value in the script,
it will cause the `cache` function to run, which will either load the cached
value or execute `c++`.
