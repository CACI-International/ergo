# Values

All values in the runtime represent a possibly-delayed result. Values are
reference counted and are only evaluated to a result at most once (so multiple
values `a`, `b`, `c` relying on another single value `d`, with any interleaving
of evaluation, will result in `d` only evaluating once).

Because of the delayed nature of the data within a value and our desire to be
able to reason about whether one value would have the same result as another,
all values have an *identity* which is derived from the literal script syntax
used to create the value as well as all captured values (such as `$v` get
expressions). A value's identity can also indicate whether the value should be
evaluated to get a more accurate identity. Value identities inherit this
indication from dependent values. The runtime automatically handles evaluating
the minimum set of values as necessary to arrive at the correct effective
identity. If two values have the same identity, it indicates that if you were to
evaluate them to their final results, those would also be the same. This
identity is essential to speeding up operations with persistent and in-memory
caching.

A value may either by delayed (without a type) or evaluated (with a type).  In
scripts, checking or matching on a type will always result in the value being
evaluated (if not already) to determine the type. In this way, you can control
when dependent values are evaluated.

Values also have arbitrary metadata that can be attached. Just like value types
define the binary format of their data, individual metadata keys define the
binary format of their values. Unlike value results, the metadata values are
immediately available without evaluating a value. Examples of value metadata
include the documentation of a value and the source location of a value. When
delayed values are evaluated, metadata keys are inherited by the result of
evaluation if the result does not set them itself.
