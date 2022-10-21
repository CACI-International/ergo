# ergo changelog

## Unreleased

## 1.0.0-rc.4  -- 2022-10-21
* Remove the force operator (`!`) and change evaluation semantics.
  * Value identities are no longer eagerly computed, but instead are delayed.
    * Changing to non-eager identities removes a lot of pitfalls with forced
      expressions, particularly with regard to recursively-defined modules which
      access each other's entries with an index operator.
  * It is now possible in the runtime to indicate that a value should be
    evaluated to compute an accurate identity (thus using the _result_ of
    evaluation, like `!` used to do).
    * This is how the features that used to be achieved with `!` will be able to
      be implemented. While reasoning about evaluation may be a little more
      difficult, this ensures the _correctness_ of identities and so generally
      users won't _need_ to reason about things as much.
* Add an lsp server.
  * Supports syntax highlighting and document formatting.
* Add support for script formatting.
  * This is still fairly simplistic, and will _only_ do whitespace and separator
    (`,` and `;`) formatting.
* Change the executable to be multi-functional, with shortcuts for subcommands
  based on the executable name.
  * Supports `evaluate` (old behavior), `lsp`, and `format` subcommands.
* Change library lookup to look in the sibling `lib/ergo` directory (rather than
  `share/ergo/lib`).
* Package with an installer to simplify and streamline installation.
* Improve the CLI markdown rendering for documentation.
* Disambiguate set and get operators.
  * Rather than using context to determine whether `:` means get or set, we
    change gets to use `$`.
* Change the string interpolation operator from `^` to `$`.
* Indirect get expressions are no longer supported. Get expressions must be with
  a string.
* Remove pattern commands as a syntax feature. Functions that should do
  something different should be renamed.
* Remove `pat` and the function version of `index`.
* Change the syntax for `{ a }` expanding to `{ :a = :a }`: it is now `{ :a }`.
* Change Maps and keyed arguments to always be evaluated, and always evaluate
  values when indexing. This avoids a lot of potential confusion when using
  maps, as you may try to insert a key or index with a value that's not a
  constant, but in the overwhelming majority the intention is to use the result
  of those expressions. If someone _does_ want a key to incorporate an
  unevaluated value, it is as simple as nesting it (for instance, in another Map
  or Array).
* Allow `Unset` types to be persisted.
* Change the value binding fallback behavior such that the bound value is
  evaluated prior to comparing identities. This, for example, makes matching on
  nested values much more intuitive: `std:match [a b c, d e f] [
  [string,other-string] -> ... ]` will evaluate `a b c` and `d e f` to see
  whether the result matches `string` and `other-string`, respectively, where
  before they would not be evaluated so it would never match. The prior behavior
  had very few and obscure use cases.
* Add a built-in `id` function to change the identity of a value.
* Fix a bug with `Iter` types where very large iterators would cause a stack
  overflow (which occurred when the value was dropped).
* Remove the `--detect-deadlock` argument. Deadlock detection is now done
  automatically with no meaningful performance implications.
* Remove the internal `store` interface from the runtime context, opting for a
  much simpler strategy that just provides the project working directory.
* Add a new syntax for setting keyed arguments (`~key`/`~key=value`) and
  disallow merging strings.
* Change the merge operator (`^`) to imply a get when followed by a string
  literal. Thus, `^$something` can be simply `^something`.
* Add `doc:raw` to get the raw documentation metadata.
* Change `doc:value` and `doc:path` to be values rather than functions.
* Add a `Functor` trait for mapping a function over inner values.
* Refactor Values to accomodate the concept of late binding (which is far less
  error-prone than dynamic binding).
  * This includes syntax for late binding (`$?BINDING`) and the built-in
    `late-bind` function.
* Allow merging `Unset` values in Commands/Blocks/Arrays (as a no-op). This is
  convenient for conditionally merging values.
* Rename `ergo` to `load`, and remove the ability to call the result with
  additional arguments.

### Breaking Changes
* Any use of `!` should be replaced with equivalent code.
* Change any `:value` get expressions to use `$value` instead.
* Change any `^...` (string interpolation) in quoted strings to be `$...`, and
  change `^^` to `^` and any `$` literals to `$$`.
* Any indirect get expressions need to be replaced with indexing or explicit
  matching.
* The overloading of a function to be used in pattern and normal contexts is no
  longer allowed (as pattern contexts don't exist). Separate the bindings to
  distinct names.
* Anything using `pat` should be changed to `fn` and/or bound to a new name.
* Anything using `index :a b` as a function rather than `a:b` can use the
  latter: `a:b` is now lazily evaluated.
* Any `{ a, b, c }` in what used to be a pattern context should be changed to
  `{ :a, :b, :c }`.
* Change `^key` to `~key`, and optionally change `(key=value)` keyed args to
  `~key=value`.
* Change `doc:value ()` and `doc:path ()` to `doc:value` and `doc:path`,
  respectively.
* Change `ergo` to `load`, and if it has additional arguments, change the call
  appropriately (e.g. `ergo script arg1 arg2` => `load script |> arg1 arg2`).

### Known Issues
* Identity calculation doesn't handle circular dependencies well. Ideally this
  would "just work", however instead a deadlock occurs. Be careful with values
  that are `eval_for_id` in `std:recurse`, for example. `std:recurse` _could_
  set the identity appropriately for the recursive value, but for now it is left
  to the user.
* Index expressions (`a:b`) will be eagerly evaluated (if all captures are
  present) when an identity is needed. This is detrimental when, for instance,
  your expression is something like `std:cache (std:exec a b c):success`. In
  this case, the intention is for `std:cache` to gate the actual `std:exec` call
  occurring, but since `std:cache` needs an identity, `std:exec` will be
  evaluated to get the `success` result due to the aforementioned behavior. In
  this case, the expression can be rewritten as `std:cache { std:exec a b c; ()
  }` or (more generally) `std:cache <| std:bind (std:exec a b c) (index
  success)`, though it is ugly.

## 1.0.0-rc.3  -- 2021-12-14
* Properly remove previously-set keys when a key is set to `unset`.
  * With the recent modification to how unset values are removed this case was
    missed.

## 1.0.0-rc.2  -- 2021-12-13
* Remove `Unset` keyed arguments to commands just like they are removed from
  maps.
* Ensure shebangs will parse correctly. They used to parse correctly when `#`
  was a line comment. Now with `#` being a tree comment, they actually do
  generally parse such that they won't fail, however to be fully correct we now
  have a special case to tokenize shebangs at the start of a file. Tokenizing
  them is useful to fully preserve the content of the source script.
* Change storage behavior to store value identities with values. This ensures
  that deserialized values have the same identities as the original values that
  were serialized.
* Improve diagnostic aggregation to properly remove duplicates.

## 1.0.0-rc.1  -- 2021-12-08
* Parallelize store implementations for Array, Map, and Iter.
* Improve type errors that involve an Error type.
  * Previously these would mention the type error and then the error itself, but
    in practice this is just extra noise and the type error should be ignored.
* Fix a minor bug in doc comment leading whitespace removal.
* Improve error message printing and information.
* Improve terminal UI rendering behavior.
* Add support for tree comments.
  * If a comment token (`#`) is followed by whitespace it is a line comment,
    otherwise it is a tree comment. A tree comment will comment out the
    following syntax tree.
* Automatically cache results of values that access dynamic bindings based on
  the binding value identities.
  * Previously, values simply checked whether they access _any_ dynamic bindings
    and, if so, would always re-evaluate. This caching is more efficient and is
    more in line with how values which don't access dynamic bindings are cached.
* Remove default doc metadata (which was present for most of the script
  primitive types).
  * Now the only doc metadata is that explicitly introduced by users.
* Improve runtime task prioritization.
  * Previously priorities were only used when a task began, but now they are
    used at any await points in tasks.
* Format markdown output in the terminal when `--doc` is used.
* Change `doc:write` output to be HTML rather than markdown.
* Remove the `:` suffix operator for no-arg function calls. While no-arg
  function calls are still possible with `f ^[]`, idiomatically it is
  recommended that no-arg calls are done by accepting a single `()` argument.
  * `doc:path` has been changed to take a single `()` argument.
  * `workspace` and `std` no longer require binding; you can access them as if
    they were loaded values (e.g., `std:import { Path } = :std` rather than `... = std:`).
  * To be idiomatic, if a function is returned as the final value of a CLI call,
    it will be called with a `()` argument rather than with no arguments as was
    previously done.
* Overhaul string literals.
  * Change interpolation in strings (previously just doc strings) to use
    `^<expr>` rather than `{{ <block> }}`.
  * Allow quoted strings to have expression interpolation.
    * This potentially replaces almost all cases of `std:String:format`.
  * Remove raw quoted strings in favor of string blocks.
    * Similar to `## ...` blocks for doc comments, string blocks look like
    ```
    ' multiline
    ' string
    ```
    They also support string interpolation, but none of the escapes that quoted
    strings do.
* Add support for attributes.
  * These look like `##<expr>`. They apply to the following value (if
    unambiguous), and will always result in an expression which binds the
    following value to the attribute value and returns the result. This is
    especially useful for adding metadata to values in an unobtrusive way.
* Add `user-configuration-directory/lib` folder as a load path.
* Change doc comments to use a dynamic binding (returned by the `doc:value`
  function) rather than the special `self` binding.
* Change `doc:child` to return both the path and the doc content.

### Migration Guide
* If you had any line comments without a space, a space will need to be added to
  retain the original meaning now that tree comments have been added. E.g.,
  `#hello world` becomes `# hello world`.
* `doc:path` calls must be changed to `doc:path()`.
* `workspace:` and `std:` must be changed to `:workspace` and `:std`,
  respectively. Other uses of the bindings ought to work as they did before.
* If a workspace `command` or script that was intended to be called from the
  command line returns a function, be sure that the no-argument case now is
  translated to something taking a `()` argument.
* Doc comment strings are more strict. They must always start with `## ` (note
  the single trailing space), and leading whitespace is no longer normalized
  based on the first line.
* Doc comment strings with `{{ ... }}` must instead use `^(...)`.
* Any doc comment or quoted strings with a literal `^` will have to use `^^` to
  escape the string interpolation.
* Raw string literals should be replaced with either block strings or quoted
  strings. In the latter case, be sure to properly escape the string content.
* Any doc comments using `self` should instead use `doc:value()`.
* The `doc:child` return type is now a map; to make equivalent calls use the
  returned `path` key.

## 1.0.0-rc.0  -- 2021-08-01
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
  `keyed` to get the `Array` and `Map` of arguments, respectively. They also
  support `Into<Array>` and `Into<Map>`, where unhandled keyed or positional
  arguments (respectively) will cause an error.
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
* Add a `--detect-deadlock` flag and functionality to detect deadlocks while
  evaluating scripts.
* Add a `bind` builtin function for binding values explicitly.
* Add an `unset` builtin value that is an Unset-typed value.
* Change the `index` builtin to also support function calls, where `index :a b`
  is the same as `a:b`.
  * This is useful to force a delayed index when otherwise an index operator
    would cause an expression to be captured (and thus evaluated ASAP).
* Add lints for unused bindings and unnecessary force expressions, and improve
  linting for string/binding conflicts.
* Add lint levels, defaulting to "on" when set without a level, and supporting
  more aggressive (but less accurate) lints with "aggressive".
* Only apply the syntax sugar that expands a string in a block/map to a set/get
  expression to unquoted strings; quoted strings will be parsed as a normal
  string.
* Allow `PatternArgs` types to be bound to `Args` types for convenience.
  * Otherwise, you'd have to use `bind (fn ^:pat-args -> ()) :args`, which is
    awkward.
* Add a `--backtrace` argument to turn on evaluation backtrace context for
  errors.

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
value). This change was made to both simplify runtime semantics but also to
allow runtime errors to be lifted to first-class types. Previously in the
runtime, a typed value might still need to be evaluated to get the value data,
however that evaluation could result in an error, and there would be no way to
transform a value typed as one type to an `Error`-typed value. Changing typed
values to strictly _not_ have any further evaluation necessary allows for the
moments where you _do_ evaluate values (which would have to be
dynamically-typed) to produce `Error`-typed values representing any errors.

#### The `Error` Type
The new `Error` type behaves just like other types. In the future it will also
support persisting so that you can cache error results as well. The type
supports `Into<Bool>`, always converting to a `false` value (just like `Unset`).

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
