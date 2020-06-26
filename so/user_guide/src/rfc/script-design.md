# Script Design

The limitations and runtime of the scripting language shape how typical scripts
will operate and what users will tend to do with them. Two features in
particular will have a large impact on how scripts are used: the presence or
absence of mutability, and the working directory when evaluating scripts. For
the sake of comparison, suppose we want to use `so` as a build system in a c++
project that has a library, executable, and test suite. For example, see the
layout here (though the layout isn't necessarily important for the
following discussion):

```
project
 |- src
 |   |- library
 |   |   |- library.cpp
 |   |   |- library.h
 |   |- executable
 |       |- main.cpp
 |- test
     |- test.cpp
```

## Mutability

### Implications
Mutability is a contentious topic. Currently, `so` scripts do not have built-in
mutability of values; you can create _bindings_, but those are not referential
bindings and one cannot change the _value_ that is bound to a particular name
(though one can bind that name to a new value, but this does not affect previous
code that bound to the previous value). So far in `so`, the scripting language
has been kept purposefully non-turing-complete, as this can make things easier
to reason about for users. Mutability might be added by a built-in function
which creates a mutable cell and allows users to change the contents with
different arguments to the function. Something like:

```
v = cell new hello
func = fn s -> v get
func . # evaluates to the string 'hello'
v set goodbye
func . # evaluates to the string 'goodbye'
```

Introducing a mutable cell as a built-in function would make the language
turing-complete. While non-turing-completeness is an admirable goal, the
advantages are not entirely clear. That being said, this form of mutability
(explicit, interior) is preferable to anything built into the parsed language,
and much of the language would remain immutable by default. It's also opt-in:
it's obvious and explicit when mutability is used.

> In thinking over this, it became clear that the runtime already is turing
> complete through loading scripts and `exec`. One can run a program with
> `exec` to dynamically write a script file, and load that file (and the written
> script can do the same). Ain't IO a pain.

### Without
If mutability is not added, the advantage is that the scripts remain
non-turing-complete, however it makes some operations awkward. In the case of
our example, if a user wanted to write a build system that could switch between
debug and release configurations (namely changing flags to compiler commands),
and wanted the system to keep configuration alongside modules (rather than all
the build logic at the top-level; useful in larger projects), then each
individual module configuration would either need to be 1) a function which
takes the toolchain and returns a module of values that use the toolchain, or 2)
a declarative form that tells the top-level scripts what to do. These options
aren't necessarily bad (and even have a few advantages themselves). The function
approach may seem awkward to those not entirely familiar with that sort of
state-threading programming.

Some disadvantages of these are:
* In the case of (1), it becomes cumbersome on users to chain module use (think
  of the test or executable module which will need to pass its function
  arguments to the library module) and refactor/feature-addition unfriendly.
* In the case of (2), adding any sort of functionality that deviates from the
  norm requires changing multiple files. That may be seen as an advantage by
  some, but it definitely can become cumbersome in even mildly complex scenarios
  if not designed well.

Without mutability, it also becomes very painful to do a few more complex
behaviors in the scripting language alone (i.e., you'd need some plugin/built-in
functionality to assist). For instance, suppose the above project used external
libraries as well, and you wanted the build system to be able to aggregate a
list of these and resolve them all at once (to solve version constraints with
total knowledge). Obviously, solving version constraints would not be
appropriate in the scripting language, however the aggregation of the list would
be, and without mutability the only option would be to pass the list contents up
and through return values in the case of (1), or to traverse growing and
changing declarative forms in (2). This example is a little contrived, but there
are likely other even more valid scenarios where state aggregation would be
useful.

## Working directory

Right now, `so` keeps the working directory as the original directory in which
it is run. If someone wants to create a build system which can be invoked within
subdirectories (as proponents of recursive make like), for example to be able to
create the library from its directory (without any extra context/direction from
parent scripts), this default has the disadvantage that anything in the module
configuration files that relies on the working directory would behave
differently if loaded in that directory versus from a parent script. Here, we
have a few options.

Users _could_ just be careful to not use anything that relies on the working
directory, and if they need that directory as a path they can get it from a
value that is put in the environment when the script is loaded. Because when
have users ever been careless about build system scripts before?

Sarcasm aside, this isn't an impractical suggestion, but it would be nice to
save the users from themselves, _and_ make the environment of a script identical
no matter from where it's loaded. Thus, it may be advantageous to always change
the working directory to that which contains a script prior to loading the
script. This will make relative file paths "just work" as many might expect. In
project-mode, it will be possible for users to use `project-root` to get the
root directory if need be, and they likely shouldn't care about what the
original directory was in which `so` was invoked (but if the argument is strong
enough, such a binding can be added to the environment as well).

In basic-mode, having the original working directory available _may_ be useful,
but it is not immediately clear how (and basic-mode isn't typically expected to
be loading many scripts spread among a file hierarchy; that's a much more
project-centric operation).

So the decision here is whether to keep the original working directory and just
provide scripts with a working directory path in their environment that they
should use, or to just change the working directory when loading scripts. This
decision has an impact on how one might invoke `so`, and on how intuitive script
behavior will be.
