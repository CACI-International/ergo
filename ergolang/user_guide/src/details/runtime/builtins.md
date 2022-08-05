# Builtins

* [ergo](#ergo) - Load a script.
* [std](#std) - Access the standard library.
* [workspace](#workspace) - Access the first ancestor workspace.
* [fn](#fn) - Match `Args` Values
* [index](#index) - Match `Index` values.
* [bind](#bind) - Manually bind a value
* [late-bind](#late-bind) - Late-bind a value
* [unset](#unset) - An `Unset`-typed value
* [doc](#doc) - Document values.
  * [doc:write](#doc-write) - Write documentation for a value to the filesystem.
  * [doc:child](#doc-child) - Write child documentation.
  * [doc:path](#doc-path) - The current documentation path, if any.
  * [doc:raw](#doc-raw) - Get the raw documentation metadata.
  * [doc:value](#doc-value) - The current value being documented, if any.
* [!id](#id) - Indicate a value's result is relevant to the identity of the
  value.


<div class="function">

## `ergo`
The `ergo` function loads a script, trying to find the passed script in the
directory relative to the working directory or in common system locations.

If there is more than one argument, the remaining arguments are used to
immediately call the resulting value as a command.

</div>

<div class="function">

## `std`
The `std` value loads the `std` library as if `ergo std` was called. Thus, you
can access values in the `std` library directly:

```ergo
std:String:format
$std
std:exec
```

</div>

<div class="function">

## `workspace`
The `workspace` value finds the first ancestor `workspace.ergo`, and loads it as
if `ergo path/to/ancestor/workspace.ergo` was called. Thus, you can access the
`workspace` directly:

```ergo
workspace:something a b c
$workspace
workspace a b c
```

</div>

<div class="function">

## `fn`
The `fn` function is used to decompose `Args` values when binding.

```ergo
f = fn :a :b :c -> ()
f 1 2 3
```

</div>

<div class="function">

## `index`
The `index` function is used to match the `Index` value when binding.

```ergo
v = index :a -> "index=$a"
v:my-ind # evaluates to "index=my-ind"
```

</div>

<div class="function">

## `bind`
The `bind` function is used to bind a value without using script syntax. For
example, `bind $a $b` is the same as `$a = $b`.

```ergo
bind-single-array-value = fn :a -> [:v] -> { bind $a $v; () }
bind-single-array-value :x = [1] # `x` is set to `1`
```

</div>

<div class="function">

## `late-bind`
The `late-bind` function is used to late-bind a value. This returns the value
with the given late bindings applied.

```ergo
val = [a,b,$?foo]
late-bind { foo = 123 } $val # evaluates to `[a,b,123]`
```

</div>

<div class="function">

## `unset`
The `unset` value is an `Unset`-typed value.

```ergo
x = [1,2,3]
x = $unset
```

</div>

<div class="function">

## `doc`
The `doc` function returns the documentation metadata attached to a value.

```ergo
## It is my value after all.
v = my-value
doc $v # evaluates to "It is my value after all."
```

### Functions

<div class="function">

<a name="doc-write"></a>
#### `write`
Write documentation of a value to the filesystem. This sets the documentation
path as returned by `doc:path` and used by `doc:child` and returns the generated
path.

```ergo
## It is my value after all.
v = my-value
doc:write my/doc/output $v
```

</div>

<div class="function">

<a name="doc-child"></a>
#### `child`
Write child documentaation of a value to the filesystem. This uses the given
relative path and the current documentation path to write the documentation and
returns the generated path.

```ergo
## Some map containing:
## [some value]($(doc:child some-value doc:value:v))
m = {
    ## Some value.
    v = val
}
```

</div>

<div class="function">

<a name="doc-path"></a>
#### `path`
The current documentation path, if set. Evaluates to `Unset` if not set.

</div>

<div class="function">

<a name="doc-raw"></a>
#### `raw`
Get the raw documentation metadata, which may be `Unset`.

```ergo
my-value = 123
doc:raw $my-value # Evaluates to `Unset` rather that a placeholder with the value type.
```

</div>

<div class="function">

<a name="doc-value"></a>
#### `value`
The current value being documented, if any. Evaluates to `Unset` if not set.

</div>

</div>

<div class="function">

## `!id`
The `!id` function is used to indicate that a value should be evaluated when the
identity is needed, and the result of evaluation will be incorporated in the
identity. The function has an optional `set` keyed argument to forcibly change
the identity evaluation semantics of the resulting value (which will propagate
to expressions using the value).

```ergo
a = std:String:from <| std:exec date
b = !id $a
std:identity $a # Will always be the same (based on the literal `std:String:from ...` syntax)
std:identity $b # Will change as the output from running the external `date` program changes

a = (!id ~set=std:Bool:false $!id) <| std:String:from <| std:exec date
std:identity $a # Will always be the same (the semantics of `$!id` are changed to not evaluate)
```

</div>
