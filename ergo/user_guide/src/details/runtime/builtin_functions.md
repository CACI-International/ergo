# Builtin Functions

* [ergo](#ergo) - Load a script.
* [std](#std) - Access the standard library.
* [workspace](#workspace) - Access the first ancestor workspace.
* [fn](#fn) - Match `Args` Values
* [pat](#pat) - Match `PatternArgs` Values
* [index](#index) - Match `Index` Values
* [doc](#doc) - Document values.
  * [doc:write](#doc-write) - Write documentation for a value to the filesystem.
  * [doc:path](#doc-path) - Get the current documentation path, if any.
  * [doc:child](#doc-child) - Write child documentation.


<div class="function">

## `ergo`
The `ergo` function loads a script, trying to find the passed script in the
directory relative to the working directory or in common system locations.

If there is more than one argument, the remaining arguments are used to
immediately call the resulting value as a command.

</div>

<div class="function">

## `std`
The `std` function loads the `std` library as if `ergo std` was called when
bound. If called as a command with no arguments (`std:`), returns the result of
`ergo std`. If bound (with an index, for example), it will bind the value to the
result of `ergo std`. Thus, you can access values in the `std` library directly:

```ergo
std:String:format
std:Path:new
std:exec
```

</div>

<div class="function">

## `workspace`
The `workspace` function finds the first ancestor `workspace.ergo`, and loads it
as if `ergo path/to/ancestor/workspace.ergo` was called when bound. If called as
a command with no arguments (`workspace:`), returns the loaded value. If bound
(with an index, command `Args`, etc), it will bind to the loaded value. Thus,
you can access the `workspace` directly:

```ergo
workspace:something a b c
workspace:
workspace a b c
```

*Note*: `workspace` only retrieves the workspace when bound, so if you want to
use workspace values in a function that may be called from outside the
workspace, those values should be bound and captured separately (i.e. call
`workspace:` to get the workspace value immediately and then use the result in
the function body). This is similar to how some of the the standard library
`script` functions behave.

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
v = index :a -> std:String:format "index={}" :a
v:my-ind # evaluates to "index=my-ind"
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

<a name="doc-path"></a>
#### `path`
Get the current documentation path, if set. Returns `Unset` if not set.

</div>

<div class="function">

<a name="doc-child"></a>
#### `child`
Write child documentaation of a value to the filesystem. This uses the given
relative path and the current documentation path to write the documentation and
returns the generated path.

```ergo
## Some map containing:
## [some value]({{doc:child some-value self:v}})
m = {
    ## Some value.
    v = val
}
```

</div>

</div>
