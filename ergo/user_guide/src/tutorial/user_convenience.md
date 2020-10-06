# User Convenience

As we wrap up this example, let's make our script more convenient for users
running it (the only change is at the end of the script):

```ergo
{{#include example/build_final.ergo}}
```

Note that a string without any following arguments (thus evaluating to a string
rather than a command) in a map will bind that value in the map's environment to
the currently-bound value, e.g. `{test}` is the same as `{test = :test}`.

We're returning a function which allows a user to run:
* `./path-to-script build` to only build the exe and library,
* `./path-to-script test` to build and run the test suite, and
* `./path-to-script release` to ensure tests pass and link the results to the current
  directory.

## Summary
Now, we have a script which exposes what we want to command-line users and
performs some fairly complex dependant commands on an as-needed basis. The C++
project here is a superficial placeholder, and hardly represents a real project
with regard to its code. However it should be clear how one might extend these
scripts for more complex scenarios and other tasks in general.
