# Values

All values in the runtime represent a possibly-delayed result. The result can
either succeed and return some data, or fail with an error. Values are reference
counted and are only evaluated to a result at most once (so multiple values
relying on a single value, with any interleaving of evaluation, will result in
the dependant only evaluating once).

Because of the delayed nature of the data within a value and our desire to be
able to reason about whether one value would have the same result as another,
all values have an *identity* which is derived from the dependent data,
functional operations, and other values which compose the value. If two values
have the same identity, it indicates that if you were to evaluate them to their
final results, those would also be the same. This identity is essential to
speeding up operations of the script with persistent and runtime caching.

A value may be either dynamically or statically typed. If dynamically typed,
this means that not only is the result delayed, but the type of the value is
delayed as well. For instance, `if` expressions produce dynamically-typed values
because they do not know which branch will be chosen until the value is
evaluated (as this is when the `if` condition is checked). The type of a value
specifies how to interpret the specific binary format of its result.

Values also have arbitrary metadata that can be attached. Just like value types
define the binary format of their data, individual metadata keys define the
binary format of their values. Unlike value results, the metadata values are
immediately available without evaluating a value.
