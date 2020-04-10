# Bind Syntax

Currently, binding values looks like

```
value = string
value = [array]
value = {map}
value = $other_value
value = (other_value)
value = (command arg1 arg2 arg3)
```

The parentheses around the value to set is required for commands (and thus
binding access as well), though this is only to be able to set a string value vs
a command. That is to say, if we were to not require parentheses for commands,
then the parser (naively) would not be able to determine whether

```
value = string
```

is setting a string or accessing a binding called `string`.

In a past version of the language, parentheses were not required and one could
do

```
value = $ string
```

to set a string (note the space after `$`, essentially applying the unit value
to `string`). This was deemed confusing syntax and was scrapped for the current
scheme.

However, there may be a happy medium here, at the cost of _very_ slight
inconsistency in the parser (though to be fair, the situations where it would be
_noticeably_ inconsistent are few).

The idea is this:
* When a script contains `value = some_string`, it will interpret this as
  setting a string value (because that's likely what the user wants).
* When a script contains `value = command arg1 arg2 arg3` (i.e. more than a
  single parsed item), it will interpret the remaining as a command (because
  again, that's likely what the user wants, and wouldn't ever make sense for a
  string).

Now, the only case which is a bit inconsistent is setting a value to another
value. If we always parsed the right-hand side as a command, this would be
`value = other_value`, but right away we have some complaints:
1. This syntax is tricky, because it's a bit of a cognitive stretch
   to realize that it's a command to retrieve the value, and
2. We've already said that this will be the way to set strings.

So, the user is forced to either use parentheses or (preferrably and very
likely) the shorthand `$other_value` syntax, leading to:
```
value = $other_value
```
which is very obvious what is going on.

So the proposal is to adopt this slight parsing inconsistency/special case so
that we may easily bind strings in the environment as well as bind the results
of commands without extraneous parentheses (because binding the results of
commands is a very high-frequency syntax and the less cruft there, the better),
_and_ force a very clear syntax for binding to other bindings (which is already
the current syntax).

## Addendum: fn syntax
One other (mildly related) situation is `fn` syntax. Right now, so a function
may return a string, calling commands must look like:

```
f = fn a b c -> (do_thing a b c)
```

When functions involve doing a series of things, e.g.,

```
f = fn a b c -> {
   thing1
   thing2
}
```

this isn't a problem, since in blocks no parentheses are required. However, I
found many times (especially when making small closures for `map`) that I'd
forget to put parentheses around the expression when there was only one.

On that note,
1. It's not clear if there's _any_ real utility in a function that returns a
   single string (so requiring special syntax for that case may be unnecessary).
2. Even if there were utility, we could follow the same convention as is
   established above for binding values.

Following the same convention as the value bindings would probably be best, as
the language would be more consistent and for the reasons stated above the
convention is probably semantically easy to remember.
