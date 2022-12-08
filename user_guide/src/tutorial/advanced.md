# Using Outputs and More Directives

Now we've done as much as we'd want for the time being in building the code.
Let's look at some more advanced usage. In particular, let's produce both a
shared library and an executable, and write/run some unit tests for the library.

## Splitting our application
Splitting up our application to use a shared library is as simple as you might
expect, except we need to have a separate link step for the library:

```ergo
{{#include example/build_split.ergo}}
```

As usual, there are a few things to note here:

* For convenience, we've created a `run-with-path` function which is a wrapper
  around `exec` that sets the `PATH` environment variable to be inherited and
  also always sequences the `exec` to ensure it completes successfully.
* We've made a separate `link` command, and use it for both `link-exe` and
  `link-so`.
* Since we are just forwarding all arguments from `link-so` to `c++`, we can
  specify the SONAME for the library (`-Wl,-h`) directly.  Likewise, we can just
  pass `:lib` to `link-exe`.
* The second-to-last value is an array. The sequencing of blocks allows each of
  the values in the array are evaluated concurrently. Then a unit value is
  returned just to make the CLI nice (there's no reason to print anything).

## Testing our library
In the C/C++ world, there's no canonical testing library. We'll use [Catch2][]
to test our library. We will download it from github, build and run its own
tests with CMake and make, and use the library to make a test program. Note that
we're only running the Catch2 tests for the sake of demonstration and
thoroughness, since Catch2 can be used as a single header-only library.

```ergo
{{#include example/build_test.ergo}}
```

Woah, that was a lot of new stuff we just added! Let's pick these changes apart.

### Improved `compile` function
`compile` has been extended to take optional include directories. We need this
to use the `Catch2` library. The `default` function from the standard library is
used to get the `includes` key from the keyed arguments, defaulting to none (an
empty array) if not specified. It then adds a `-I` argument prior to them using
`Iter` functions.

### Getting Catch2
We've added a whole block to retrieve Catch2 and run its unit tests. The block
is used to scope the inner variables and keep our environment cleaner. Here, we
use the standard library `net` module to fetch and unarchive the catch2 source.

After unpacking, we build the tests with `cmake` and `make`, using the `pwd`
keyed `exec` argument (as `run-with-path` forwards all arguments to `exec`) to
set the working directory when running the programs.

The final line of the block ensures that the tests have been run before
returning the Catch2 include path, and caches this completion so that we don't
run tests every time we retrieve the value (we only need to run the test suite
once to be reasonably confident in its correctness!).

### Test program creation and execution
The final change we made is that we compile a test program using the new
`compile` function feature and the Catch2 library, and run it before copying our
outputs in the last line. This is fairly straightforward and follows from what
has been previously discussed (like passing `~includes=[$Catch2]` to the
`compile` function). We do, however, need to make a temporary library directory
so that our test executable can find the libraries it needs. The `libpath` block
handles this: it copies the library into a new directory with the expected name
based on our SONAME setting, and returns this library path. Then we set the
`LD_LIBRARY_PATH` environment variable appropriately when running the test
program.

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
