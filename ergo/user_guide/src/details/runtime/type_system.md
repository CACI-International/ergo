# Type System

The script language is dynamically and strongly typed. The type system includes
the concept of type classes/traits, which define functionality implemented over
the type. Types and traits may include arbitrary runtime information, including
nested types.

While the language _is_ dynamically typed, most functions check types as soon as
possible and are applied immediately in scripts, so the runtime behaves
optimistically as a statically-typed language.

## Script Types
The following types are most often created directly using script syntax:

### Unit
```ergo
()
```
The unit type, similar to void types in some other languages (but actually
returned as a value, unline those other languages which have special rules
around void types).

### String
```ergo
mystring
"my string"
```
The string type, for strings of utf-8 characters.

### Array
```ergo
[]
[1,(),hi]
```
The array type, which contains a sequence of values (with arbitrary type).

### Map
```ergo
{}
{a=1,b=2}
```
The map type, which contains a mapping from keys to values (both with arbitrary
type).

### Unbound (Function)
```ergo
:a -> :a
```
The unbound type, which may be bound to a value with any expression that
performs a bind (bind expression, command, pattern command, index, etc).

### Args
```ergo
f a b c
```
The argument type, which is implicitly created in command expressions to group
arguments to a command (in the above, `a`, `b`, and `c`).

### Pattern Args
```ergo
f a b c = 1
```
The pattern argument type, which is implicitly created in pattern command
expressions to group arguments to a pattern command command (in the above, `a`,
`b`, and `c`).

### Index
```ergo
a:b
```
The index type, which is implicitly created in index expressions to bind the
index value (in the above, `b`) to the target.

### Unset
```ergo
{}:a
```
The unset type is used in the script runtime when a value is missing for a
number of general operations, like when indexing a map without a matching key
(like above).

## Other Types
In addition to the types that can be created using script syntax, there are a
number of other types (and plugins can add more types) that exist, like `Path`,
`Bool`, etc. These (as well as script types) can be created and manipulated
using functions from the standard library.
