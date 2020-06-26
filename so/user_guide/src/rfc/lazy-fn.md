# Function Semantic Consistency

Right now, the built-in functions are not consistent in how they behave. All
functions (both user-defined and built-in) are called immediately, and the
runtime applies them to get the resulting Value from the call.

If functions were lazily applied, static analysis would be required on
user-defined functions to discover, and built-in functions would have to define,
the resulting value type. This type must be known since the type is necessary
for further processing logic. Moreover, if other arbitrary metadata were added
to values, this metadata too would have to (somehow) be exposed without calling
the function, which is likely impossible.

For this reason, functions are immediately applied, and _values_ in the
scripting language are what are lazy. Note that functions _are_ first-class
values, however this lazy evaluation is only with regard to the function itself,
not with regard to its application.

However, just because functions are immediately applied does not imply that how
they treat their arguments is the same. By their nature, user-defined functions
treat arguments the same as all other built-in functions that they use (and
recursively, other user-defined functions that they used). For this reason we'll
only consider the built-in functions.

## Built-in functions inconsistently handle arguments

Currently, the built-in functions don't all handle arguments the same. Some
immediately evaluate arguments, while others use arguments to create new values
that depend on them (and evaluate some functional application that is delayed
until their returned value is needed). The built-in functions that immediately
evaluate arguments do so because, at the time of their writing, it seemed like
they would/should only be used in that way. However, in practice, it has become
clear that they may be useful in a delayed-evaluation context as well.

The built-in functions that immediately evaluate values include:
* `exec` - most things that matter are not immediately evaluated, however the
  `env` and `description` settings are immediately evaluated.
* `fold`/`map` - the original thought was that, since these operate on script
  types, they should perform their operation immediately and return a value that
  is content-identified.
* `fs glob` - it was thought that this would only be useful with a
  content-identified return value, since it would typically be used to get lists
  of files. However, it has become apparent that this would definitely be useful
  in non-immediate contexts, like globbing things unpacked from a command (but
  not forcing the unpack in the process).
* `has` - like `fold`/`map`, since it is only used on script types, it seemed
  like it would only be useful to operate immediately.
* `load` (`so`) - this necessarily must evaluate its argument since it cannot
  know the return type/value without loading the module.
* `track` - immediately evaluates the argument to check the file; necessary
  since the point of this function is to create a value that is
  content-identified by a file.
* `variable` - immediately evaluates the first argument if it is a string, to
  check for `--deps`.

That's a large portion of the functions, and what's more, most are unnecessarily
immediately evaluating things based on preconceptions about their use cases. It
should be possible for _all_ functions except `load` and `track`, because of
special properties of their return values, to delay evaluation. Furthermore, all
future functions should also delay evaluation if their return semantics allow
it.

> Aside: `track` is a bit old/rigid; it may be more useful to specifically have
> a `file` type which is meant to represent a file that exists (as opposed to
> paths which may not represent an existing file/directory), and also is
> content-identified by the file contents rather than the file path. Likewise
> you could have a `directory` type.

## Maintaining existing functionality
For some of these functions, the fact that the returned value is
content-identified is important. For this reason (and others not discussed
here), to be able to maintain functionality when these functions are made to not
immediately evaluate arguments, it will be necessary to add a `value` function.
The `value` function _will_ always immediately evaluate its argument, and return
a new value which is identical to the value argument and is identified by the
content of the value argument as well. In this way, script users will be able to
choose when values are to be identified by the content itself versus by the
dependency chain, and furthermore they will be able to force values when
necessary.

## Conclusion
Consistent, _predictable_ behavior is important, and so unless a function is
supposed to be doing something 'special' with regard to value identification, it
should not be immediately evaluating any arguments.
