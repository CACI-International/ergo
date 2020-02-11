# Using Outputs and More Directives

Now we've done as much as we'd want for the time being in building the code.
Let's look at some more advanced usage. In particular, let's produce both a
shared library and an executable, and write/run some unit tests for the library.

## Splitting our application
Splitting up our application is as simple as you might expect, except we need to
have a separate link step for the library:

```sh
{{#include example/build_split}}
```

As usual, there are a few things to note here:

* For convenience, we've created a `use_path` variable which sets the `PATH`
  environment variable to be inherited.
* Since we are just forwarding all arguments from `link_so` to `c++`, we can
  specify the SONAME for the library (`-Wl,-h`) directly to the function.
  Likewise, we can just pass `$lib` to `link_exe`.
* We've made a separate `link` command, and use it for both `link_exe` and
  `link_so`.
* The final value is an array. Because of this, each of the values (both futures
  in this case) in the array are retrieved concurrently.

## Testing our library
In the C/C++ space, there's no canonical testing library. We'll use [Catch2][]
to test our library. We will download it from github, build and run its own
tests with CMake and make, and use the library to make a test program. Note that
we're only running the Catch2 tests for the sake of demonstration and
thoroughness, since Catch2 is a single header-only library.

```sh
{{#include example/build_test}}
```

Woah, that was a lot of new stuff we just added. Let's pick these changes apart.

### Improved `compile` function
`compile` has been extended to take optional include directories. We need this
to use the `Catch2` library. The first line of our new function maps on all
arguments to the function, looking for any that contain a `includes` key (using
the builtin `has` command). For those that do have the key, it creates arrays of
arguments to pass `-I [path]` for each path specified.

### Getting Catch2
We've added a whole block to retrieve Catch2 and run its unit tests. The block
is used to scope the inner variables and keep our environment cleaner. Here, the
first three commands fetch the release tarball with `wget`, make a directory in
which to unpack it, and unpack it.

The `mkdir` command is using a new `exec` directive, `dir`. This is fairly
similar to `file`, except the value bound to the given variable name (in this
case `unpack`) is actually a function which can be called on any number of path
components to make new directory functions. A final `.` as an argument will
instead return the path created up to that point.
  
The `tar` command is also using some new `exec` directives. `pwd` is simple
enough: it sets the working directory of the process. `creates` is a bit
smarter: it binds variables to the given paths for each (as a map), but will
only run the command in question for each path if the path does not yet exist.
So in this case, the `sourcedir` variable is bound to the `Catch2-2.11.1`
directory within the unpacked tarball contents. The variable will always
evaluate to this directory path, however it will also run the `tar` command if
it does not yet exist.

After unpacking, we build the tests with `cmake` and `make`, using a few more
new features: the `do` command and the `once` key returned by the `exec` command:

* The `do` command simply takes an array of futures, and creates a new futures
  that will run each sequentially (as long as no failures occur) and return the
  value of the final future. This is useful to make futures depend on others in
  a less precise way when you don't want to use values from commands to track
  dependencies (here, we could have used the test binary output that we expect
  be created by `make`, but chose not to).
* The `once` key returned by `exec` does what one might think: it ensures that
  the command has been run once and succeeded. This avoids unnecessary
  repetition of commands with side-effects.

The final line of the block is another `do` command. In this case we want to run
the tests once to ensure our library is sane, and then return the include
directory for Catch2 (which was available directly in the tarball we unpacked).

### Test program creation and execution
The final change we made is that we compile a test program using the new
`compile` function feature and the Catch2 library, and run it before copying our
outputs in the last line. This is fairly straightforward and follows from what
has been previously discussed (like passing `{ includes = [$Catch2] }` to the
`compile` function). We do however need to make a temporary library directory so
that our test executable can find the libraries it needs. The `libpath` block
handles this: it creates a directory, copies the library into it with the
expected name based on our SONAME setting, and returns this library path. Then
we set the `LD_LIBRARY_PATH` environment variable appropriately when running the
test program.

For reference, the `test.cpp` file contains:

```c++
{{#include example/test.cpp}}
```

## Final thoughts
This script may seem like a lot to write, but remember that this is just a
tutorial; many of these functions would and should be abstracted into reusable
libraries or plugins of commands, to ensure consistency and/or provide more
granular control over behavior.

[Catch2]: https://github.com/catchorg/Catch2
