# Functions

This is a proposal to change function declaration syntax, to make it more
natural and readable.

## How things work now
```sh
dist = fn seq {
    map (fn exec ln -f $(@ 0) (path dist (@ 0))) $@
}
```

## First argument to 'fn' is a pattern to match
This is also a partial specification of a pattern-matching syntax; a builtin
`match` operator may prove useful to perform pattern-matching on arbitrary
values.

If a macro system were added, `match` could be implemented using this system.
However, this is likely unnecessary complexity.
```sh
dist = fn args seq {
    map (fn [a] exec ln -f $$a (path dist $a)) $args
}
```

## Instead of 'fn', use backslash for functions
The thought here is that it's very uncommon to need a literal backslash, and
most shells/languages need escaping of a backslash anyway, so that would be
familiar to most people (right now, you don't need to escape backslash).
```sh
dist = \args seq {
    map (\[a] exec ln -f $$a (path dist $a)) $args
}
```

## Consider what syntax might look like with non-positional arguments
If commands are extended to allow non-positional arguments, functions should be
isometric and allow non-positional arguments. Thus, syntax might look like the
following:
```sh
dist = \[^args] seq {
    map (\{key=binding,^restkeys} [a,^rest] exec ln -f $$a (path dist $a)) $args
}
```

Or, since non-positional arguments should always(?) be optional, they could be
an implicit binding (in this example, `fnkeys`).
```sh
dist = \args seq {
    map (\[a,^rest] exec ln -f (fnkeys $a) (path dist $a)) $args
}
```

## Maybe function parameter declarations should look similar to application
This would be more intuitive, I think. If this is the case, there needs to be a
dividing symbol to indicate the end of the arguments (in this example, an
arrow).
```sh
dist = \^args -> seq {
    map (\^{key=binding} a ^rest -> exec ln -f $$a (path dist $a)) $args
}
```

Or, if we *really* want it to be isometric with command invocation, we would use
`$` or `(..)` to indicate non-literal arguments. Literal arguments would be
useful for specifying anchors to divide multi-argument captures.
```sh
dist = \^$args named $name -> seq {
    map (\^{key=$binding} $a ^$rest -> exec ln -f $$a (path dist $name $a)) $args
}
```
In this case, if `dist` were invoked as `dist abc def named something`, `args`
would be bound to the array `[abc,def]` and `name` would be bound to
`something`.
