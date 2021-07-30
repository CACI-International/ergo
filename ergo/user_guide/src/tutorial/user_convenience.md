# User Convenience

As we wrap up this example, let's make our script more convenient for users
running it (the only change from our previous version of the script is at the end):

```ergo
{{#include example/build_final.ergo}}
```

`match:value` returns a function which allows a user to run:
* `./path-to-script build` to only build the exe and library (also just
  `./path-to-script` with that first `fn:` case),
* `./path-to-script test` to build and run the test suite, and
* `./path-to-script release` to ensure tests pass and link the results to the current
  directory.

We also have added a doc comment to this value which documents the command-line
interface.

## Summary
Now, we have a script which exposes a nice interface command-line users and
performs some fairly complex dependant commands on an as-needed basis. The C++
project here is a superficial placeholder, and hardly represents a real project
with regard to its code. However it should be clear how one might extend these
scripts for more complex scenarios and other tasks in general.
