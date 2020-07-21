# Type System

The script uses a dynamically-, strongly-typed language. The type system
includes the concept of type classes/traits, which define functionality
implemented over the type. Types and traits may include arbitrary runtime
information, including nested types. Within scripts, users are generally unaware
of traits and types; they are used primarily by functions to type check
arguments.

While the language is dynamically typed, since functions check types as soon as
possible and are applied immediately in scripts, the runtime behaves similarly
to a statically-typed language.

Individual values are always delayed futures; the types are immediately
available and wholly describe what to expect as a result when the future is
forced. Values also have an immediately-available identity, which is tied to the
resulting future: value identities are meant to be reproducible and there should
generally be a bijection between identities and resulting futures. This
bijection can be broken in certain circumstances, based on the semantics of the
value. That is, the identity/future bijection should be defined based on the
value semantics. For instance, `path new` always returns the same identity, even
though the underlying path will be random on each execution. This is because the
semantics of `path new` are that it gives you _some_ new, unused path. The path
itself doesn't matter.

Values also have arbitrary metadata that can be attached. Just like value types
define the binary format of future data, individual metadata keys define the
binary format of their values. Unlike value future data, the metadata values are
immediately available.
