# User Convenience

As we wrap up this example, let's make our script more convenient for users
running it (the only change is at the end of the script):

```sh
{{#include example/build_final}}
```

We're returning a map with keys that can be accessed on the command-line. In
order, a user can run:
* `build build` to only build the exe and library,
* `build test` to build and run the test suite,
* `build release` to ensure tests pass and link the results to the current
  directory, and
* `build` to link the results to the current directory.

Here, `*` is a (somewhat) special key which indicates that, if no explicit keys
are provided, the map should evaluate to that value. In other words, it's a
default value to use. Note that this behavior only applies to the final script
value; the `*` key is a normal key like any other in all other situations.

## Summary
Now, we have a script which exposes what we want to command-line users and
performs some fairly complex dependant commands on an as-needed basis. The C++
project here is a superficial placeholder, and hardly represents a real project
with regard to its code. However it should be clear how one might extend these
scripts for more complex scenarios and other tasks in general.
