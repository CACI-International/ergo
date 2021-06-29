# ergo changelog

## 1.0.0-rc.0  -- Unreleased
Most of the changes listed below are breaking changes, so there is no separate
section for those. They also could all probably be considered as new features or
improvements, so no such distinction is made.

* The runtime has been completely refactored and rewritten, with many lessons
  learned from prior version. This refactor results in different semantics for
  the scripting language, which are described below. A number of syntax changes
  were made as well, but in general the syntax is almost identical to before.
* Removed the `BindEqual` parsed expression type, as it was barely ever used and
  prevented other more intuitive use cases (and could be implemented as a
  function).
* Removed the `If` and `IfBind` parsed expression types, as they can be
  implemented as functions now.
  * The reason they were parsed was so that the condition expressions could have
    delayed evaluation; now that everything has delayed evaluation, this is
    unnecessary.
* Parsing was changed to make bind statements (note the nomenclature: _not_
  expressions as before) only valid in blocks and commands.
  * You can no longer bind values in arrays. They do not have their own binding
    scope.
* A merge of an unbound value in a function pattern (e.g. the `rest` in `fn :a
  ^:rest -> ...`) will now be bound with an `Args`- or `PatternArgs`-typed
  value, which captures both the positional and keyed arguments that remain. If
  more than one is present, the _first_ gets the keyed arguments, whereas
  positional arguments are grouped and matched as before. This is far more
  elegant than the prior `fn a ^:rest ^{^:kw}` forms.
* The `Args` and `PatternArgs` types support indexing with `positional` and
  `keyed` to get the `Array` and `Map` of arguments, respectively.
* A merge of a `String`-typed value in a command or a block will bind the item to
  a unit value, e.g. `a b ^c` will be the same as `a b (c=())`, and `{ ^v }`
  will be the same as `{ v = () }`. This is useful shorthand for toggle flags.
* Blocks are now used to sequence values/operations (see `Evaluation
  Semantics`) since evaluation is delayed.
* A merge of an `Array`-typed value into a block will sequence the items in the
  array as if they were items in the block inserted at the merge point.
* Raw quoted strings are supported. Some number of repeated single quotes are
  used to open a raw quoted string, and the same number will close one. They do
  not interpret any escape sequences.
  * For example: `'my raw \quoted\nstring'`, `''This has a literal ' in it.''`
* Quoted and raw quoted strings simply change the tokenizer mode, such that if
  they follow eachother or normal string tokens without spaces, they will be
  considered part of the same evaluated string.
  * For example: `e"="mc'^'2` evaluates to the string `e=mc^2`.
* Directories will now only resolve to `dir.ergo` when being loaded
  (`workspace.ergo` is now strictly used for workspace resolution rather than
  also being used in directory resolution).
* `<|` now has lower precedence than `|`/`|>`, as it is used much more in
  practice and this is the more useful behavior.
* Dynamically-scoped bindings are supported by the runtime. These are bindings
  which are present for any values which are evaluated after they are set. The
  runtime does not account for these when calculating value identities, so care
  must be taken to use them correctly based on the intended semantics.
* All script evaluation happens concurrently as tasks on the thread pool (with a
  fixed, high priority).
* A new `Number` type has been added to the runtime, supporting arbitrarily
  large rational numbers.
* Add support for arbitrary escaped unicode characters in quoted strings.
  * This looks like `"\u{123456}"`, where you can have from 1 to 6 hex digits.
* Do not cache value results if the dynamic scope is accessed when the value is
  evaluated.
  * This reduces surprise when using dynamic bindings in scripts (and makes
    those always correct semantically).
* Always pass command-line arguments as Strings to faithfully preserve shell/OS
  semantics.
  * The first argument is treated a little specially, interpreting any `:` as
    index operations into the loaded value.
  * If `--expression` is used, arguments are passed as-is (and may have
    operators, other script types, etc).

### Evaluation Semantics
The new evaluation semantics and data model are a drastic simplification over
the old runtime. Now, _all_ syntactic evaluation is delayed. What this means is
that, for example, if you have a script with the following:
```
f = fn :a -> std:String:format "Hello, {}" :a
f World
```
When you load this script, a dynamically-typed value is returned which
represents the execution of the top-level block. When that is evaluated (once),
it binds `:f` to a dynamic value that will evaluate to the function as written,
and then returns a dynamic value which will apply `f` on `World`. Once that
value is evaluated again, it will bind `World` to `:a` in the function, and
return a dynamic value that will apply `std:String:format`, and finally when
evaluated again `format` will be called and a `String`-typed value will be
returned.

Importantly, throughout all of this evaluation the runtime tracks subexpression
captures such that the dynamically-typed values have identities that are derived
from (parts of) the literal syntax and the bound values. The implication is that
value identities still accurately and uniquely represent the value that would be
returned if the expression is fully evaluated to a typed value.

#### The Force (`!`) Operator
The `!` symbol is no longer an evaluation-level operator. It is instead a
syntax-level operator, indicating that the following expression should be
evaluated _as soon as possible_ rather than being delayed in a value. The old
runtime implementation made `!` very confusing to use (but often necessary).
Now, it should be much clearer when to use it. In particular, `!` should _only_
be used to affect a dependent identity (because the identity of the evaluated
value will be used), or to make some side-effect happen at a certain time. _As
soon as possible_ pertains to binding availability: `fn :a -> !std:log:debug :a`
will evaluate `std:log:debug :a` as soon as the function is called (normally it
would simply return a delayed value which would later perform the log).

Generally, `!` will be less prevalent, and it should be somewhat obvious when to
use it. You also should never really need to use it when writing many classes of
functions where resulting identities of delayed values are not that relevant (or
more accurately, only depend on inputs), e.g. data-manipulation functions. These
can be considered pure functions with regard to value identity, in the sense
that the output identity strictly depends on the input identities.

#### Data Model
Values have changed in the runtime such that a dynamically-typed value can be
evaluated to return a new (possibly dynamically-typed) value, and typed values
represent a value which cannot be further evaluated (though a typed value _can_
contain other values which can then be evaluated, e.g. items in an `Array`-typed
values). This change was made to both simplify runtime semantics but also to
allow runtime errors to be lifted to first-class types. Previously in the
runtime, a typed value might still need to be evaluated to get the value data,
however that evaluation could result in an error, and there would be no way to
transform a value typed as one type to an `Error`-typed value. Changing typed
values to strictly _not_ have any further evaluation necessary allows for the
moments where you _do_ evaluate values (which would have to be
dynamically-typed) to produce `Error`-typed values representing any errors.

Types and traits still exist as before and operate in the same ways.

## 1.0.0-beta.9.1  -- 2021-04-05
### Improvements
* Evaluate scripts concurrently to avoid stack overflows.
* More efficiently calculate type/trait uuids at runtime.

## 1.0.0-beta.9  -- 2021-03-09
### New Features
* Make indexing a distinct operation (as opposed to being syntax sugar).
  * Indexing (infix `:`) is now a distinct operation, binding an `Index`
    value. In scripts, you may match this with `index :v`. Maps and arrays
    have been changed to use `Index` rather than `Args` to get values.
* Add `Unset` type and use it when map indexing fails, patterns are unmatched,
  or map pattern keys are unmatched.
  * This allows for cleanly dealing with missing/optional map keys.
* Add a global `std` function as shorthand for `(ergo std)`.
* Change how workspaces are loaded.
  * There is no special `prelude` key anymore, and it is not specially loaded.
  * A global `workspace` function is shorthand for `(ergo
    path/to/ancestor/workspace.ergo)`, and the workspace should be accessed
    through this.
  * The `command` key is still special in the workspace, however only because
    the command-line invocation of `ergo` will use `workspace:command <args>` if
    `ergo <args>` does not resolve to a file. Otherwise, `ergo` (the load
    command) will _only_ load a file based on its path and the load path.
    Previously, it encapsulated the workspace fallback behavior, but this is no
    longer the case.
* Support negative indices in array indexing (which will index from the end of
  the array).
* Add `-E`/`--error-limit` flag and default to 3 frames of error context.
  Pretty-print error tracebacks rather than relying on the error display
  implementations.
* Add `--doc-write` flag to output documentation to markdown files.
* Add a built-in `doc` function to get documentation of a value, supporting the
  following additional indexes:
  * `doc:write` to write out documentation for a value,
  * `doc:child` to write child documentation (if `doc:write` is being used), and
  * `doc:path` to get the current output location for documentation, if any.
* Support expressions in doc comments.
  * `{{ ... }}` in doc comments (possibly spanning multiple lines) will evaluate
    the content as a block, with scope that is shared throughout the doc
    comment. The result of the block is displayed inline in the final comment.
* Add `Iter` type for iterators.

### Improvements
* Improve doc comment parsing by not requiring a space after the initial `##`.
* Change parsing of parentheses to be purely grouping.
  * Previously, parentheses implied commands. Now they are strictly for
    grouping, e.g. `(a)` and `a` parse identically. Commands are inferred
    based on the presence of arguments. To call a command with no arguments,
    use the trailing colon, e.g. `a:`.
  * This makes things work better especially with regard to pipe operators,
    where you can now use parentheses as the limits of the pipe operator without
    need for calling a command. For instance, `a b |>:d | e` parses the same as
    `e ((a b):d)`, and if parentheses imply a call, then it ends up calling the
    result of `(a b):d`, which is likely not what is desired. Now, that is
    equivalent to `e (a b):d`, as the extra parentheses around the single value
    are superfluous.
  * The interpretation of values within parentheses is now identical to those
    without; if the value is a single value, it is the result, otherwise if
    there is more than one value, the first is bound to an `Args` containing the
    rest to get the result.
* Improve parsing of the pipe operators (`|>`, `|`, and `<|`) to make them truly
  rewrite macros. Previously they were parsed as recursive descent binary
  operators, which had limitations and special code for certain situations. For
  instance, `a b |>:<| c d` would not work as expected (but now does).
* Change command-line parsing to stop parsing flags at the first non-flag
  argument.
  * This is more convenient than using `--` to end the parsed arguments, and
    generally makes more sense.
* Make pattern errors more accurate. Previously any error while executing
  pattern expressions were considered pattern errors, but this shouldn't be the
  case.
* Do not use `-F` with less on mac for paging; mac has an old, buggy version of
  less where this doesn't work. `more` does work with this flag on mac (and
  actually is less), but using `less` as the default is still probably more
  useful.
* Improve error messages a little to decrease ambiguity/repetitiveness.
* Allow `ByteStream` types to be displayed. This was a nuisance; initially the
  idea was that there would be a `Utf8Stream` type to specifically capture such
  a stream that's appropriate for display, but we sanitize the bytes in
  `ByteStream` when converting to a `String`, so there's no reason to not do the
  same to implement `Display`.
* Release the load cache when no other load calls are present, to correctly drop
  lifetimes of values.
* Make `if` return `Unset` if the false case is not provided and the condition
  evaluates to that case.

### Bugfixes
* Fix incorrect parsing of indexing in e.g. `a |>:b:c`.
* Fix incorrect parsing of trailing colon in e.g. `a b:c:`.

### Migration Guide
Syntax has changed in the following breaking ways:
* Parentheses are purely grouping, and do not imply a command call. Thus, if you
  were calling a command with no arguments as `(function)`, it _must_ be changed
  to `function:`.
* Indexing is a distinct operation (`a:b` is no longer syntax sugar for `(a
  b)`). If you were using `:` on values that were not arrays or maps, that
  must be changed to be parenthesized. If you were using a function
  call to get map/array values (`map :key`), that must be changed to use the
  infix `:` instead.
* Pipe operators are parsed directly after bind expressions. Previously, they
  were parsed after function (`a -> b`) expressions. This means that you can use
  the pipe operators to group a function, however you must be careful if using
  the rightward operators (`|` and `|>`) in the body of a function, as now that
  will group things regardless of the `->` (arrow has higher precedence now).
  For example, `:a -> a b |> c` used to parse the same as `:a -> (a b) c`, but
  now it will parse as `(:a -> a b) c`.
* Workspace access no longer relies on `prelude`. In any script, if you use the
  global `workspace` function (whether indexing or calling), it will load the
  nearest ancestor workspace and then index/call it with the arguments (if any).
  This means anything using values from a `prelude` will now have to either load
  the prelude explicitly in the scripts (`^workspace:prelude`, for instance), or
  will have to access values directly in the `workspace`.
  * This was done to make workspace use more obvious and simple.
* Workspace fallback behavior (with `command`) is now _only_ a special behavior
  of the command-line invocation. If you were relying on such behavior in
  scripts, use `workspace:command <args>` explicitly in the script.

Semantics have changed in the following breaking ways:
* `()` (unit-typed values) now convert to true (rather than false) if converted
  to a `Bool`. This means the logic of some things that relied on this behavior
  needs to be updated. In particular, all values now convert to true if there is
  no other override, and `Unset` values convert to false. You can (and should)
  use the standard library to explicitly create `Bool` instances.
* `if` returns `Unset` for the false case if it is not provided. Previously, it
  returned `()`.

## 1.0.0-beta.8  -- 2021-01-15
* Fix some bugs and improve output UI.
  * Don't count/display abort errors.
  * Fix rendering reset.
  * Don't count prelude load errors.
  * Don't allow ctrl-C to affect child processes.
  * Don't echo stdin in fancy formatting.
* Correct closure lookup.
  * When a closure-bound value evaluated to a string in command position, it was
    looked up twice (incorrectly).
* Support doc comments (starting with `## `).
* Major syntax and runtime updates:
  * Remove use of `pom`, opt for a custom parser.
  * Remove `match` as a built-in parsed expression (it will be added to `std`).
  * Remove patterns as an AST concept. Everything is parsed as expressions,
    including bind targets (`[target] = ...` and `[target] -> ...`).
  * In bind targets:
    * `a = b` is parsed as a bind equality (which will
      bind a value to both `a` and `b`), 
    * `!a` will cause `a` to be parsed like a normal expression,
    * `:a` is parsed as a set expression (which will then bind to a value)
  * Support calling a function with no arguments using a trailing `:`:
    `a:` is the same as `(a)`.
  * Commands and arrays now correctly create new scopes, just like blocks.
  * Support setting keyword arguments in commands with bindings:
    ```
    cmd (:kw = something)
    ```
  * Support bind expressions as `if` conditionals (`if (:a = b) :a other`),
    where the branch is chosen based on if the binding succeeds.
  * Change function definition to be generalized to any bindings, and `fn` is
    just a function which will binds to call arguments.
  * Add `pat` as a function which will bind to bind-call arguments (i.e.
    arguments from within a binding context):
    ```
    a = pat :v -> ...
    # Now we can use `a` within a binding.
    a :x = 1
    ```
    versus
    ```
    a = fn :v -> ...
    # The following will error.
    a :x = 1
    ```
  * Merge expressions are always immediate (previously array merges produced
    delayed values).
* Apply linting while executing values.
* Remove `script-dir` and `load-path`.

### Migration Guide
The syntax changes should not require too much effort to enact. In particular:
* In patterns, `a = ...` is a shortcut for `:a = ...`, so this simple case need
  not be changed in scripts.
* In patterns, any nested bindings will need a preceding `:` now. This will
  affect nested array and map patterns:
  * `[a,b]` should now be `[:a,:b]`.
  * `{a}` is still fine (and is the same as `{:a = :a}`), and likewise because
    of the above shortcut `{a = b}` in a pattern can be changed to either `{:a =
    :b}` or `{a = :b}`.
* __Important__: Nested array and map patterns used to be delayed (i.e., the
  source array/map value wouldn't be destructured until one of the nested
  elements was accessed). This is now _not_ the case; patterns are applied
  immediately, and thus any destructuring will occur immediately. This was done
  to simplify the behavior, though with more thorough design with regard to
  other pattern functions, it may change back to being delayed in the future.
  For now, if you were relying on this behavior to be delayed, you should access
  elements directly by index, e.g. `map:key` (which is still delayed).
* `match` is now a function in the standard library, so what was before
  ```
  match [[value]] { [[pattern]] = [[expression]], [[pattern2]] = [[expression2]], ...}
  ```
  should now be
  ```
  std:match [[value]] ([[pattern]] -> [[expression]]) ([[pattern2]] -> [[expression2]]) ...
  # Or, to keep things line-separated
  std:match [[value]] ^[
      [[pattern]] -> [[expression]]
      [[pattern2]] -> [[expression2]]
  ]
  ```
  Note that the pattern/expression pairs are just normal functions (which is why
  `match` can itself be a function).
* `fn` is now a binding function. Syntactically this doesn't mean much (it still
  looks like `fn [[args]] -> [[expression]]`), but it does mean that function
  arguments all need `:` preceding them, just like binding patterns:
  `fn a b ^rest -> ...` should now be `fn :a :b ^:rest -> ...`.
  * This also means that if you had a function _without_ arguments (`fn ->
    ...`), this needs to be changed to either `(fn) -> ...` or `fn: -> ...`.
* Patterns no longer support the `=[expression]` form for literal comparison.
  Instead, strings are literals by default, and any other literal value can be
  preceded by a `!`: `!` in patterns means to interpret the following expression
  as if it were a normal expression, not applying the special interpretation
  rules of pattern bindings. Note that this mainly changes the interpretation of
  `:v`: in a binding, `:v` means to set `v` in the current scope, whereas `!:v`
  means to use the value in `:v`.
* In patterns, commands are interpreted as bind commands. Previously, `(a b c) =
  1` would match the literal output of `(a b c)`. Now, you need `!(a b c) = 1`
  to achieve the same effect. It's unlikely this was used much at all though...
* Array merges now occur immediately, so any code relying on their previous
  delayed behavior will need to be updated.
* Instead of `:script-dir`, use `(std:script:dir)` (it's a function).

## 1.0.0-beta.7  -- 2020-12-08
* Add `-e`/`--expression` flags to evaluate the arguments as an expression
  without preceding `ergo`.
* Fix evaluation when pipe operators are present in the command-line arguments.
* Change the first `:` to `|>:` in command-line arguments for convenience.

## 1.0.0-beta.6  -- 2020-12-07
* Fix log pausing to correctly clear the rendered content.
* Fix mac plugin detection.

## 1.0.0-beta.5  -- 2020-12-07
* Improve task tracking and UI display of running tasks.
* Change the application log to be written to the data local directory. Support
  `ERGO_LOG_FILE` environment variable to override this.
* Improve error logging to accurately track pending errors.
* Add `-d`/`--doc` flags for getting documentation for the final value.
* Add `-p`/`--page` flags for paging the final output.
* Improve `ergo` use in scripts to not always apply functions when they are
  loaded. Instead, the final value at the top level will, if a function, be
  applied (with no arguments) to refine the final value.
* Support calling `ergo` without arguments. This evaluates to the `command`
  workspace key.

## 1.0.0-beta.4  -- 2020-11-17
* Add the `!` operator, which may precede any expression or argument to evaluate
  the result immediately.
  * If the expression evaluates to a dynamically-typed value, it will evaluate
    the value to the inner value once.
  * If the expression evaluates to a typed value, it will shallowly evaluate the
    value by content.
* Add `if` as a syntactic operator.
  * Now the `if` cases are conditionally evaluated, as opposed to the prior
    function which evaluated them immediately.
* Ensure parenthesized commands allow newlines between arguments.
* Properly (and minimally) capture environment variables in fn/match/if blocks.
* No longer rely on environment bindings when loading scripts.
* Rename `work-dir` to `script-dir` in script default bindings to more
  accurately describe the value.
* Allow merge operator (`^`) to prefix command expressions without
  parentheses.
* Add `-c`/`--clean` command-line parameter to clear the storage directory prior
  to execution.

## 1.0.0-beta.3  -- 2020-11-4
* Support interactive subprocesses by suspending logging while they run.
* Support dynamically-typed values, and change indexing, array merging, match
  expressions, and set expressions to lazily perform their actions.
  * When values _are_ constants or derived from constants, they are still
    eagerly evaluated.
* Fix a critical soundness bug around plugin unloading that was causing crashes.
* Change indexing to require the indexed value to exist (rather than returning a
  unit value).
* Add a warning when a string literal is used which matches a key in the
  environment. This is only enabled with the `--lint` flag, and when this flag
  is present no evaluation of the final value is done.
* Change maps to be keyed based on values rather than string literals (a
  superset of prior functionality).
* Change workspace interactions:
  * `prelude` is now loaded directly as if the ancestor workspace was specified.
    This is a subtle difference, but differentiates prelude loading from
    fallback behavior.
  * Fallback behavior now first applies `command` to the ancestor workspace, and
    whatever is returned by that will receive additional arguments to the load
    command (rather than applying these arguments directly to the ancestor
    workspace return value).

## 1.0.0-beta.2  -- 2020-10-07
* Change script syntax, adding pipe operators and indexing/accessing bindings
  with `:`.
* Support calling functions without arguments.
* Add support for `dir.ergo`, which behaves like `workspace.ergo` (and is
  preferred) when a directory path is provided, but otherwise behaves like a
  normal script.
* Change storage command-line option to use furthest ancestor workspace.
* Add syntax highlighting files in `contrib` for vim and highlight.js.
* Use better syntax highlighting in user guide.
* Improve error output.
* Add `self-file-path` pre-defined binding at script entry.

## 1.0.0-beta.1  -- 2020-09-24
* Change to tokio runtime.

## 1.0.0-beta.0  -- 2020-09-21
* Initial release.
