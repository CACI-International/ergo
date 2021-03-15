# Script Execution Semantics

Currently, to get scripts to run quickly and correctly, one must be careful to
reduce unnecessary function calls and carefully craft values (with extra
cognitive load to track what a value's id might represent). This is _difficult_.
From the perspective of a typical scripting language, the usual approaches might
be to add an interactive debugger, profiling capabilities, compile to byte code
and run on a vm (for speed), etc. However, this being a lazily evaluated
scripting language, there are a few more options open to us.

## Considerations
It is important that we make scripts behave in a predictable manner, but also in
an optimal manner when possible. After all, the purpose of ergo is to get things
done as quickly (wall time) as possible. All other features are built around
this tenet, including not evaluating values that aren't needed, identifying
values so they can be deterministically cached, etc.

A few approaches to achieve these goals will be discussed, including:
* delaying expression evaluation,
* evaluating expressions concurrently,
* assuming the purity of functions, and
* deduplicating live values at runtime.

## Expression Evaluation
One thing that can probably be confusing about ergo scripts is that some things
seem delayed (anything that is represented by a value) while other things are
immediate (like applying functions, merging into maps, etc). There have been
various design decisions that have led to this behavior, but the largest was
that, ideally, if there is a problem in the script itself, it comes to the
surface as soon as possible.

It is a difficult struggle between priorities, because on the one hand this is a
dynamically-typed and lazy language, so one might not encounter type errors
until later anyway just because of the nature of evaluation, but on the other
hand if there is a problem in the script it'd be nice if it came to the surface
prior to some long operations that may occur (and aren't cached). That said,
it may be more consistent to not type check as soon as possible. This desire is
what has led to parts of evaluation occurring immediately while others don't. In
particular, block evaluation and _function application_ are really the parts
which can unnecessarily slow down execution. Right now, all built-in and
standard library functions, when called (eagerly by the runtime), type check the
arguments and often return the new value which represents the actual business
logic of the function.

As function application currently stands, there are two important things that
occur:
1. the arguments are checked or converted (when possible) immediately, so if
   there are incorrect types an error occurs immediately, and
2. the _identity_ of the resulting value can be determined immediately, and thus
   if there are any special semantics of the function that dictate the value
   identity, those can be imbued immediately.

This gives functions the freedom to impose different semantics of the operation
into the returned value. For instance, _most_ functions generally return a value
which simply depends on the input arguments and the function itself. This is
very common, and relates to the functions being pure and injective from a
mathematical sense: a function's output is uniquely determined from inputs.

However, this can be confusing as some special functions have semantics that
deviate from the norm (those that have extra arguments that are purely used for
side-effects), like `std:task` (which returns a value with the same identity as
is passed to it, but takes a description argument as well). Now, it makes sense
that `std:task` behaves in this way, and may not be too difficult to wrap one's
head around, but other functions may not be so clear.

Likewise, it can be annoying that things like merging a map or matching a
pattern always happen immediately (both from a script user perspective and as a
runtime implementor). If these were delayed in the same way as _all
expressions_, the behavior would be much more predictable.

For these reasons (and more to be discussed shortly), it may be best to
fundamentally change how script evaluation occurs. Since we are in a dynamic,
lazily-evaluated language, evaluation can look like this:
1. Parse expression (technically parse the whole AST up-front; this is fast)
2. Return a possibly-dynamically-typed value (depending on the expression) that,
   when evaluated, will evaluate the outer-most node of the expression, passing
   any inner node as delayed values that will parse _those_ expressions.
In this way, no matter what the _only_ script expressions that are evalutaed
will be those that are used, since necessarily we will be passing them around as
values.

For many script expressions, the type of the returned value will be known
immediately (like maps, strings, arrays, unit, etc). Otherwise (e.g. for command
expressions), it will necessarily be dynamic.

### Advantages
* This will likely result in some serious speed-up of script execution overall.
  For instance, since the standard library was made to have script components,
  it takes about 0.2s to load (it used to be on the order of nanoseconds, just
  loading a DSO). As the standard library script portions grow, this will become
  more of a problem. One part of this is that the expressions still need to be
  recursively traversed to capture bindings correctly; this process will likely
  be a good place to focus some optimizations of the runtime. However, if we
  aren't evaluating things that aren't needed for the most part only the bits
  and pieces of the standard library that are actually _used_ will be evaluated
  (which generally may be a somewhat sparse selection).
* This change would make it almost trivial to evaluate scripts concurrently, as
  we just need to spawn the evaluation onto the async runtime.
* Related to the last point, it should become fairly easy to load scripts that
  cyclically depend on parts of each other (as long as there are no cyclic
  _data_ dependencies). Right now, cyclic loading of scripts files is explicitly
  prevented as it will cause a stack overflow.
* The `if` syntax primitive will be able to be a function instead (since it was
  useful to have something that didn't even evaluate a branch). Likewise, other
  user functions can have semantics like `if` (where some code isn't evaluated
  at all in certain circumstances).

### Disadvantages
* Type checking will still occur, but its use as a hint that something will fail
  before evaluating values will be lost. Types will still be used for
  verification and better errors. It's already pretty easy and common to have
  type checking delayed until later in the runtime (types in the body of a
  function are not checked until the function is called), so this may not be
  quite as much of an issue as it seems.
* For the many cases where functions are truly pure functions, the identity will
  be correct. But for cases like `std:task`, if it is not forced with a `!` (to
  make it occur immediately), the resulting value will depend on the task
  description (which is probably never desirable?).

### Compromises
* The disadvantages above could be avoided if we still always evaluate commands
  immediately, but _only_ commands (and pattern commands, and _maybe_ indices?).
  This means that something of the form
  ```ergo
  my_func = fn :a -> { ... }
  ```
  would actually evaluate the function, but the _block_ expression inside the
  function would not evaluate immediately. Basically the larger slowdown of
  always evaluating functions right now is that if the inner expression has
  multiple expressions (like a block), those all evaluate immediately too. It
  would be faster to only apply commands, and then things like type casting
  could occur immediately (so we would get some immediate type checking back).
  This would not be as consistent behavior, though.
* We could also distinguish functions that execute immediately versus those that
  don't. This is also somewhat inconsistent but would be convenient as far as
  the script syntax goes. Maybe functions that execute immediately should follow
  a naming convention (much like Rust's macros all having a trailing `!` to
  invoke them, or naming conventions often seen in lisps). Due to the nature of
  how things are bound, this would probably rely on users correctly naming
  things, which may be undesirable.
* Rather than relying on names, we could simply rely on the `!` operator to
  evaluate the commands immediately. Then, this would be consistent with the
  above (i.e. commands don't evaluate immediately) but you could make them
  evaluate immediately using a `!` operator. This might end up a little ugly
  (i.e. things like `std:task` and types would pretty much always have a `!`
  prefix) but it would be very consistent and obvious.

### Proposal: Remove map merging into blocks
To be able to correctly capture environment values in inner scopes, it may be
necessary to prohibit merging maps into the scope of blocks. This is somewhat
undesirable as this would differentiate blocks and maps more (where they have
been kept somewhat interchangable). Though, maybe they _shouldn't_ be
syntactically so similar; it's not that common to want to change a map into a
block or visa versa (often just for debugging to expose more inner values or to
restrict the returned values to a subset). For capturing values
correctly/efficiently, they would need to be explicitly named. `^std:` clearly
doesn't explicitly name the things being brought into scope. For similar human
reasons, explicitly naming what's brought into scope may be for the best; it
makes it obvious from where a particular name originates when reading a script.
This was an elegant duality between maps and blocks that has been convenient,
but it may have run its course.

### Case Study: When to force values (value identities)
A common confusion (among script writers and even the ergo developers) is the
clarity of value identities. The value identity is integral to optimizing
performance (both with runtime caching and disk persistence). However, it is not
something explicitly represented in script syntax, and thus one must rely on
documentation _and_ memory of behavior/semantics/inconsistencies to manage the
identity correctly.

When writing scripts, one must think both about what operations/manipulations of
data need to be done, but also how they should depend on each other. That is,
when someone writes a script, they need to decide what changes in values (like a
different input value, a change to a command in the script itself, etc) should
be considered relevant to the returned value. For example, the semantics of
`std:task` are such that the `description` argument is not relevant to the
resulting value. Likewise, any use of `std:variable` is changing (explicitly)
what is relevant to a resulting value. While `std:variable` may seem like an
answer, it would be incredibly cumbersome for users to explicitly annotate what
each value depends upon.

This needs to be made more obvious. If at all possible, the syntax should make
it somewhat clear what dependencies are present. _If_ the runtime is changed
such that all evaluation is delayed, the `!` (force) operator may be the answer.
All cases have not been thoroughly thought through here, but it's likely that
one could read a script and simply determine the dependencies as any expressions
that _aren't_ forced. When writing scripts, it may still take a bit of extra
thought and care to make sure you force what should not be considered a
dependency, but overall that will probably be much easier to understand than how
scripts behave now.

## Parallelization of Evaluation (better concurrent execution)
Currently a lot of opportunity for speedup is dropped on the floor as evaluation
is done sequentially. A few parts rely on serial evaluation (like environment
access), but this _could_ be improved with correct environment capture
propagation. If all expression evaluation is delayed, it would also be fairly
easy to parallelize their execution. It's not clear whether the `-j` argument
(limiting the number of concurrent tasks) should affect evaluation; that is
typically used to prevent a program from taking up the entire computer's
resources, but on the other hand that's not really a good solution to the
problem in the first place (if on a shared server, a shared resource pool should
be used; if running locally, people very often do not care). Also, script
execution should not at all be the dominant part of the overall program
execution with regard to CPU use, so it might not be applicable.

### Proposal: Map/Block order does not matter (to aid in parallelization)
This idea has been kicking around for a little while, but it'd be interesting if
blocks were changed to be identical to maps, except there is some special syntax
or a special key which indicates the final value (rather than the last
expression being a binding or not). If this were the case, maps/blocks _could_ be
changed so that order doesn't matter. That way, one could access values defined
before _or after_ the current line in a script. This would complicate how
environment access works (just a little), but would make it possible to
evaluate the keys in maps in parallel pretty easily (since the environment
access would be consistent). Incidentally, this is how haskell (and a few other
functional languages) does lookup. There is one consequence though, which may be
a strong reason to _not_ do this: name uniqueness. It is somewhat common to
redefine a binding, e.g.:
```ergo
a = something
a = [:a, something_else]
```
Redefining a binding is useful for two reasons: avoiding name creep (in haskell
you often see `a`, `a'`, `a''` as binding names to "redefine" a value), and
specifically indicating that the _old_ binding will no longer be used. The
former is a matter of opinion/cleanliness for the most part, but the latter is
actually very useful, and moreover if you can't redefine a binding then you
can't set it to `Unset` explicitly either. So if this change were done, some
common idioms would have to change.

Note that rust distinguishes item (top-level) scopes from expression scopes,
where the former is order-independent and the latter is order-dependent. This
could loosely be correlated to ergo maps versus blocks, but if maps and blocks
behaved differently for environment access that may be confusing (since they
share the same syntax). Also note that one use case of this (apart from making
parallization more explicit) is an organizational one: for instance, it may be
desirable to put the external interface and its documentation at the beginning
of a script file for easy reading (instead of having it at the end of the file
as occurs now).

## Value Deduplication
If we can assume that all functions are pure, we could also assume that values
are pure. That is, we can assume that a value with a particular identity will
always have the same result as another with the same identity. This, combined
with some form of delayed execution of at least most types of expressions (even
if not commands), would end up more-or-less memoizing all command calls and
value manipulations. This would be satisfying as it would automatically
optimally execute values, sharing results where possible. Importantly, some
things (like work/progress tracking) rely on value lifetimes, so the runtime
cache of values would have to hold weak references to the values. Thus, it would
only deduplicate "live" values in a given execution session.

A few functions will not work under these assumptions as they are currently
implemented. For instance, `std:Path:new` returns a value that always has the
same identity, but evaluates to a distinct Path. You would not want to
deduplicate those values as they are now, that would result in unrelated code
sharing a single `Path` (and very likely interacting with it in incompatible
ways)! To correct this, we may want to change `std:Path:new` to return an
identity based on source location and/or a hint/seed, so that distinct instances
have different (but consistent) identities. Alternatively, we could distinguish
_value identity_ from _runtime identity_, where the latter would account for
differences in behavior (relevant for value deduplication) whereas the former
would represent (possible) differences in the evaluated value (relevant for
persistent caching).

## Conclusion
The following decisions need to be made:
* Should the evaluation of expressions be delayed? If so:
  * Should commands still always evaluate immediately, or
  * should there be two types of functions, some that execute immediately and
    some that don't (and how would one indicate this in scripts).
  * Should merging maps into block scopes be prohibited (to facilitate
    environment capture in delayed-evaluation expressions)?
  * Are there any deeper consequences of assuming the purity of functions? Would
    there be a situation where you'd want a function to always be called
    repeatedly for side effects, or would that generally not be in the spirit of
    ergo scripts not repeating work?
* Should script evaluation be parallelized? If so:
  * Would there be any consequences to users as far as errors/behavior goes?
  * Should map/block syntax change to allow using any binding within the current
    scope or outer scope (regardless of order of declaration)?
* Should all live values be deduplicated?
