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

* The `std:import ...` at the top inserts keys of the standard library into the
  top-level environment. Specifically, `std:` returns the standard library and
  `std:import` is a function which, when bound to a value, gets keys from the
  bound value. This makes it convenient to use standard library functions
  without always preceding them with `std:`.
* `cache` is a function which persists its argument to disk in a cache based on
  the value identity (roughly speaking, a hash of the syntax and captured
  bindings). Thus, if the identity changes from dependencies or code changing,
  it will cache a new value. If the identity does not change, the cache will
  find the value and load it _rather_ than evaluating the value.
* `Path:new` is a function which produces a new value which will be a random
  path. You can use it to create a new path on disk in an intermediate
  directory.
* `fs:track` is a function which takes a path and derives the identity from the
  file contents. Thus, if the file contents change, the value changes.
* `fs:copy` is a function which will copy a file or directory (recursively) to a
  target file or directory.
* `source:dir` is a function that returns the parent directory of the source
  file where the argument is from. The `()` is a unit value; since the value is
  created in _this_ script file, the parent directory (and thus the directory
  returned by `source:dir`) is the parent directory of this script.
* The `!` syntax operator indicates that we want to run the following expression
  **as soon as possible**. What this means is that instead of the `cache` call
  using a value derived from the literal syntax of the `fs:track ...` calls, it
  will be derived from the captured value resulting from the calls. Thus, if the
  content of the tracked file changes, the identity will change, and the `cache`
  call will be based on a different identity (and will either have a cache hit
  for that identity if the files have had that content before, or will rerun the
  command to cache the result).

> You probably noticed the `|>` operator. This is a _pipe_ operator, which is
> syntax sugar to group code. There are three such operators: `<|`, `|>`, and
> `|`. `<|` groups the right-hand side, so for instance `a b <| c d` is the same
> as `a b (c d)`.  `|>` groups the left-hand side, and `|` groups the left-hand
> side and moves it to be the last argument of the right-hand side.

So, in the above binding of `exe` in the script, we:
1. create a new path,
2. run the `exec c++` command with tracked inputs (so the expression has an
   identity that changes if the contents of `main.cpp` or `lib.cpp` change) and
   an output using the new path we made, accessing `complete` of the returned
   map from `exec` to wait for the command to complete,
3. return the new path as the last value of the block,
4. cache the entire block, so that the cached identity will depend on the
   contents of `main.cpp` and `lib.cpp`, and the literal syntax in the script
   (so changing those files or the script within that block will rerun the
   command).

Finally, we copy the (cached) `exe` path to a local, named file.

## Why do we need `Path:new`/`fs:copy`?

If we just left the same `-o forty_two` argument to the `c++` command, our path
caching wouldn't work reliably on cache hits. We certainly could do something
like this if we wanted those sorts of semantics (i.e. once there is a cache miss
the old cached value is lost), however in this case we want to keep things
simple and just always cache the result of the different compiles individually.
This is where `Path:new` comes in, as it gives us a new, unique, non-existing
path to which to write.

The generated paths are stored in a local `.ergo_work` directory, so to get a
user-friendly version of the path we can either link or copy it from there. Thus
we use `fs:copy` to get it, and since this is the last value in the script, it
will cause the `cache` function to run (to get `:exe`), which will either load
the cached value or execute `c++`.
