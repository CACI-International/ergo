# Builtin Functions

* [ergo](#ergo) - Load a script.
* [std](#std) - Access the standard library.
* [workspace](#workspace) - Access the first ancestor workspace.
* [fn](#fn) - Match `Args` Values
* [pat](#pat) - Match `PatternArgs` Values
* [index](#index) - Match `Index` Values or manually index a value
* [bind](#bind) - Manually bind a value
* [unset](#unset) - An `Unset`-typed value
* [doc](#doc) - Document values.
  * [doc:write](#doc-write) - Write documentation for a value to the filesystem.
  * [doc:child](#doc-child) - Write child documentation.
  * [doc:path](#doc-path) - Get the current documentation path, if any.
  * [doc:value](#doc-value) - Get the current value being documented, if any.


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
:std
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
:workspace
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

## `pat`
The `pat` function is used to decompose `PatternArgs` values when binding.

```ergo
f = pat :a :b -> :v -> ()
f :x :y = 1
```

</div>


<div class="function">

## `index`
The `index` function is used to match the `Index` value when binding.

```ergo
v = index :a -> "index=^a"
v:my-ind # evaluates to "index=my-ind"
```

It can also be used to manually index a value (rather than using script syntax).
This makes the index operation always delayed rather than it possibly being a
capture expression based on the semantics of the `:` operator.  For example,
`index :a b` will result in the same value as `a:b`.

</div>

<div class="function">

## `bind`
The `bind` function is used to bind a value without using script syntax. For
example, `bind :a :b` is the same as `!:a = :b`. This makes the bind operation
always delayed rather than executed immediately as normal bind statements are.

```ergo
bind-single-array-value = pat :a -> [:v] -> bind :a :v
bind-single-array-value :x = [1]
```

</div>

<div class="function">

## `unset`
The `unset` value is an `Unset`-typed value.

```ergo
x = [1,2,3]
x = :unset
```

</div>

<div class="function">

## `doc`
The `doc` function returns the documentation metadata attached to a value.

```ergo
## It is my value after all.
v = my-value
doc :v # evaluates to "It is my value after all."
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
doc:write my/doc/output :v
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
## [some value](^(doc:child some-value (doc:value () |>:v)))
m = {
    ## Some value.
    v = val
}
```

</div>

<div class="function">

<a name="doc-path"></a>
#### `path`
Get the current documentation path, if set. Returns `Unset` if not set.

```ergo
doc:path ()
```

</div>

<div class="function">

<a name="doc-value"></a>
#### `value`
Get the current value being documented, if any. Returns `Unset` if not set.

```ergo
doc:value ()
```

</div>


</div>
