# ergo changelog

## Unreleased
### Standard Library
#### Additions
* Add `std:fs:read-link` to read symbolic link targets.

## 1.0.0-rc.4  -- 2022-10-21
### Language/Runtime
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

#### Breaking Changes
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

#### Known Issues
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

### Standard Library
#### Bugfixes
* Fix a bug where `std:net:http` would still try to download the file when
  present in the `ERGO_NET_CACHE` (but would still return the cached result).
* Allow multiple tasks to correctly be suspended when awaiting the same task.

#### Improvements
* Allow `Unset` values in the `env` map of `std:exec`.

##### Additions
* Add `std:Iter:partition` to partition iterators into a Map of Arrays.
* Add `std:Iter:chunks` to iterate over consecutive chunks of a given size.
* Add `std:Iter:new` to create an iterator from a generator function.
* Add `std:Iter:true` as a convenience to filter values to those that convert to
  true (useful in combination with `std:Iter:map` as a filter map).
* Add `std:Iter:item` to get the next item from an iterator.
* Add a `Type` type, and refactor how script types work in general.
  * You can now get the type of any value as a runtime value.
* Add `std:trait`, which allows access to built-in traits (`Bind`, `Into`,
  `Stored`, `Functor`, and `Display`) as well as creating custom traits with
  `new`.
* Add the `std:Type:or` function (which was already in the last release but was
  mistakenly not exported).
* Add `std:Type:name` to get the name of a type.
* Add `std:Type:get` to get the Type of arbitrary values.
* Add `std:Type:basic` to get a basic version of a type suitable for comparison.
* Extend `std:doc:module` to handle `Type`s too.
* Add `std:Iter:unzip` to handle what used to be done with `std:zip` in pattern
  expressions.
* Add `std:fs:file-size` to get the size of a file in bytes.
* Add `std:fs:rename` to rename files and directories.
* Add `std:fs:lock` to get advisory locks on files.
* Add `std:env:concurrent-tasks` to provide the runtime task concurrency.
* Add `std:env:project-dir` to get the project working directory.
* Add `std:env:temp-dir` to get the system temporary directory.
* Add `std:env:process-id` to get the ergo process id.
* Add `std:env:process-dir` to get the process working directory.
* Add `std:Cache` to create caches (both in-memory and persisted to disk).
  * Caches now use sqlite to store values.
* Add `std:once` to add values to an in-memory cache.
* Add `std:Path:owned` to mark a path as owned, which will delete the path when
  the value is no longer used. The cache takes ownership of these paths.
* Add `std:Path:for`, which is very similar to the previous
  `std:Path:with-output` but is doing something slightly different, so it's
  worthwhile to have a new name.
* Add `std:index` to get indices of a value. It is similar to `std:import` but
  doesn't support recursive indexing and doesn't error on Unset results.
* Add an `all` keyed argument to `std:Iter:zip` to replace missing values with
  `Unset` rather than ending the iterator.
* Add `std:optargs` as a convenient way to add optional arguments to a function.
* Add `std:dynamic:bind` to deeply bind dynamic bindings in values. This is
  preferable to `std:dynamic:eval` as it leads to fewer mistakes when working
  with dynamic bindings.
* Add `std:Unset:or` to return the first non-Unset value.
* Add `std:Unset:index` to deeply index a value, possibly returning Unset if any
  intermediate index is Unset.
* Add `std:load-lazy` to lazily load a file (causing the identity of the
  expression to not rely on the loaded file value).
* Add `std:backtrace` to log a backtrace of the current evaluation.
* Add `std:next` to evaluate a value once and bind the result.
* Add `std:eval-id` to fully evaluate a value when the identity is calculated.

##### Modifications
* Removed the `std:by-content` function, as one shouldn't be reinterpreting
  value identities.
* Change `std:meta:eval` to return a map with the evaluated value and the
  metadata value (if any). This also allows binding the map so that the
  evaluation occurs immediately, which is often the intention (since forcing
  evaluation is no longer supported syntactically).
* Remove `std:eval` in favor of `std:Type:any`.
* Rename `std:String:format` to `std:String:match` (making it only valid to
  match strings rather than creating strings).
* Rename various functions in `std` to be consistent. See Migration for details.
* Remove `std:default` behavior in normal (as previously designated)
  expressions.
* Allow converting `ExitStatus` to `Number`.
  * If the process exited as the result of a signal, `-1` is returned.
* Change `std:exec` in a number of ways:
  * Remove the `stdin` keyed argument; stdin is passed with a function.
  * Print all information of the command in logs/errors, including env variables
    and working directory.
  * Return a new `Child` type which supports:
    * `stdin` index - a function to pass data to the child process stdin
    * `stdout` index - a `ByteStream` of the process stdout (as before)
    * `stderr` index - a `ByteStream` of the process stderr (as before)
    * `exit` index - a `ExitStatus` (renamed from the previous `exit-status`)
    * `success` index - runs the process until completion, requiring success and
      producing an error with the exit status, stdout, and stderr on error
      (renamed and improved from the previous `complete`)
    * conversion to `Bool`, `Number`, `String`, and `ByteStream`
* Remove `std:env:get` in favor of `std:env:vars`, which is a `Map` of all of
  the process's environment variables.
* Rename `std:env:user-cache` and `std:env:system-cache` to `std:env:user-dir`
  and `std:env:system-dir`.
* Remove the `no-persist` option from `std:cache` (use `std:once`).
* Support caching top-level errors in `std:cache` by setting the `allow-error` keyed
  argument to `top`.
* Remove `std:Path:new` and `std:Path:with-output`.
* Improve `std:import` to not evaluate values when binding keys.
* Generalize `std:Function:recursive` to `std:recurse`, which allows creating
  arbitrary recursive values. Use with caution!
* Support merging `Args` in `std:merge`.
* Improve `std:fs:copy` to allow non-existent `from` paths if making a symbolic
  link.
* Rename `std:ergo-remote` to `std:load-remote`.
* Remove `std:variable` since the builtin `id` does the same thing.

#### Migration
* Remove the use of `std:by-content`. If it was being used to compare nested
  structures, some other means should be used (like binding in a `std:match`).
* Change `std:meta:eval` calls to properly get the value. You likely want to
  bind the result so that the evaluation occurs immediately, e.g.
  ```
  { result, metadata-value } = std:meta:eval KEY :value
  ```
* Replace `std:eval` calls with `std:Type:any`. In patterns, they are
  interchangeable. In normal expressions, `std:eval :v` can be converted to
  `{std:Typed :v = :v; :v}`, though you may no longer need `std:eval` semantics
  at all.
* Anything using a custom `std:type:new` type will need to be rewritten for the
  new API. This includes changing any `bind` keyed argument to instead call
  `std:trait:Bind:impl`, and adding associated functions using
  `std:Type:modify`.
* Any type checks as normal (as previously designated) function calls will need
  to be rewritten or wrapped.
* Change `std:String:format ...` to equivalent formatted strings `"..."` in
  normal (as previously designated) expressions, and to `std:String:match` in
  pattern expressions.
* Change `std:Iter:zip` in pattern (as previously designated) expressions to
  `std:Iter:unzip`.
* Change `std:default X as Y` in normal (as previously designated) expressions
  to `std:Unset:or X Y`.
* Rename the following functions:
  * `std:Any` -> `std:Type:pass`
  * `std:Typed` -> `std:Type:any`
  * `std:Array:Values` -> `std:Array:values`
  * `std:Array:Of` -> `std:Array:of`
  * `std:Map:Values` -> `std:Map:values`
  * `std:Map:Of` -> `std:Map:of`
  * `std:env:user-cache` -> `std:env:user-dir`
  * `std:env:system-cache` -> `std:env:system-dir`
* Update the use of `std:exec`, passing `stdin` with the returned function and
  changing `exit-status` to `exit` and `complete` to `success`.
* Change `std:env:get ENV_VAR` to `std:env:vars:ENV_VAR`.
* Change `std:cache ^no-persist :value` to `std:once :value`.
* Replace uses of `std:Path:new` and `std:Path:with-output` with
  `std:Path:for`, which will produce deterministic paths.
* Change `std:Function:recursive <| fn :self ...` to `std:recurse <| :self -> fn
  ...` and change any use of `self` in the function body to not pass `self` as
  the first argument.
* Change `std:ergo-remote` to `std:load-remote`.
* Replace `std:variable ~depends=DEPS VALUE` with `id ~set=DEPS VALUE`.

## 1.0.0-rc.3  -- 2021-12-14
### Language/Runtime
* Properly remove previously-set keys when a key is set to `unset`.
  * With the recent modification to how unset values are removed this case was
    missed.

### Standard Library
* No changes.

## 1.0.0-rc.2  -- 2021-12-13
### Language/Runtime
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

### Standard Library
* Add `std:io:is-terminal` to check whether a stream is connected to a terminal.
* Add `std:fs:file-type` to get the file type of a path.
* Change `std:cache` to not cache errors by default.
  * One can use the `allow-error` flag to cache errors if that behavior is
    intended.
* Change `std:net:unarchive` to show network errors prior to unarchiving.

## 1.0.0-rc.1  -- 2021-12-08
### Language/Runtime
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

#### Migration Guide
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

### Standard Library
#### New Features
* Add `std:Error:display` to convert Errors into Strings.
* Add `std:test` module.
  * Add `std:test:run` as a basic way to nicely run tests and display the
    results.
* Add `fallback` keyed argument to `std:match`.
  * This is more ergonomic for cases where the final case may error (but that
    error should be returned).
* Add the `std:sync` module for synchronizing concurrent evaluation.
* Add `std:Iter:count`.
* Add ordering utilities:
  * `std:Order` - ordering constants
  * `std:Number:compare` - compare numbers
  * `std:String:compare` - compare strings
  * `std:Iter:order` - order an iterator according to an ordering function
* Add `std:Path:from`, and allow Strings to be converted to Paths.
* Add `std:merge` to deeply merge maps and arrays.
* Add basic math operators (`+`, `-`, `*`, `/`, and `%`) to `std:Number`.
* Add `std:equal` to compare two values by identity.
* Add `std:Bool` functions:
  * `not` for complements
  * `and` for conjunction
  * `or` for disjunction
* Add `std:net:url-encode` for url-encoding strings.
* Add a `pretty` keyed argument to `std:json:stringify` to make the output
  string nicely formatted.
* Add `std:meta:eval` to evaluate a value until a metadata key is present.
* Add `std:env:config` which evaluates to the configuration directory.
* Add `std:net:cache-dir` to return the specified network cache directory, if
  any.
* Add `std:net:http-defaults` to return the user defaults.
* Rework the `std:doc` module to use attributes and automatically document
  module contents.
* Add `std:Typed`. It will evaluate the value and reject any Errors.
* Add `std:Function:module` to improve the process of creating functions that
  also act as modules (with indices).

#### Improvements
* Do not cache `std:net:unarchive` results, to make the standard library purely
  functional.
* Cache `std:ergo-remote`, as that is an appropriate place for caching of the
  downloaded libraries and `std:net:unarchive` no longer caches things.
* Allow all `std:net:http` arguments to be used with `std:ergo-remote`.
* Allow `std:fs:copy` to copy symlinks rather than follow them by default.
  * Add a `follow-symlinks` keyed argument to change this behavior.
* Improve `std:Path:or-source` to error when non-Path values are passed.
* Change `std:doc:functions` to generate per-function doc children.
* No-arg functions are changed to take a single `()` argument to be idiomatic.
* Change type functions to use the `@` index for binding
  construction/destruction.
* Add `else` keyword and `else if` additional cases to `std:if`.
* Change `std:match` to reject Errors by default, and add an `allow-error` flag
  to allow errors (matching the old behavior).
* Support an `enter` keyed argument to `std:net:unarchive` to return the inner
  path if the unarchived content has exactly one top-level entry.
* Support an `ERGO_NET_CACHE` environment variable for caching any body content (by
  url) downloaded with `std:net:http`.
* Support user-configurable http defaults in `std:net:http` to allow e.g.
  user-specific per-endpoint authentication.
* Allow `Unset` and `Unit` values to be passed as arguments to `std:exec`. They
  will be ignored.

#### Bugfixes
* Fix error return values when using `std:eval`.
* Fix the `std:fs:glob` relative directory.
  * It was mistakenly using the call site script path rather than the parent
    directory.
* Fix the behavior of `std:require` when a value is Unset, and ensure that
  documentation is carried over through `std:import` uses.
* Correctly capture network client errors in the runtime.
* `std:String:format` patterns would match string prefixes rather than whole
  strings; this has been fixed.

#### Migration Guide
* Change the following functions from `function:` to `function ()`:
  * `std:Path:new`
  * `std:io:stdin`
* Change all type construction/destruction to use `Type:@` rather than `Type:`.
  * This includes _all_ types in `std` as well as any types created with
    `std:type` functions.
* Add an `else` keyword to `std:if` calls when necessary.
* Add `^allow-error` to `std:match` calls where matching errors is intentional
  (there probably aren't many cases of this).
* Any documentation using `std:doc` must change to use the new functions
  (attributes). Existing documentation likely can take advantage of the new
  features as well.

## 1.0.0-rc.0  -- 2021-08-01
### Language/Runtime
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

#### Evaluation Semantics
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

##### The Force (`!`) Operator
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

##### Data Model
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

##### The `Error` Type
The new `Error` type behaves just like other types. In the future it will also
support persisting so that you can cache error results as well. The type
supports `Into<Bool>`, always converting to a `false` value (just like `Unset`).

Types and traits still exist as before and operate in the same ways.

### Standard Library
#### New Features
* Add `std:env:arch` to get the host architecture.
* `std:log` uses the `Display` ergo trait to write values, which is far more
  useful than requiring a `String` argument.
* `std:dynamic` has been removed as all expressions are dynamically-typed,
  however a new (unrelated) `std:dynamic` module has been added, supporting
  interaction with dynamic bindings.
* `std:typed` has been renamed to `std:eval`, since getting a typed version of a
  value is the same as evaluating the value.
* `std:seq` is removed as sequencing values is a syntax-level feature.
* `std:error` is removed as errors are first-class types.
* `std:script` is removed as the runtime no longer supports getting script paths
  in this way.
* Add `std:if`, which works the same as the old built-in (though without
  equivalent IfBind support).
* Add `std:source` module to get/manipulate source information of values.
  * `path` returns the source path, if available.
  * `dir` returns the parent of the source path, if available.
  * `copy` copies the source information of one value to another.
  * `map` maps a function on a value but retains the original value's source.
* Add `std:net:http`, which is mostly HTTP-feature-complete.
  * `std:net:download` still exists, but is implemented using `std:net:http` and
    now supports all keyed argments that `std:net:http` does.
* Support `ByteStream` values in `std:fs:unarchive`.
* Add `std:fs:archive` for creating archives (opposite of `std:fs:unarchive`).
* Add `std:json` module with `parse` and `stringify` functions to convert json
  strings to ergo values and visa versa.
* Add `std:String:chars` to get the characters in a string.
* Support using `std:String:format` as a pattern command to do simple
  destructuring of strings.
* Add `std:getopt` and `std:getopt:fn` to support simple conversion of string
  arguments containing long getopt-style flags to `Args`.
* Add `std:identity` to get the identity of a value.
* Optimize the behavior of `std:fs:track` to share results throughout a single
  execution (and not block calling threads), and add a `force-check` flag to
  allow forcing a file to be checked regardless of whether it was already
  checked (in case the file changed while scripts were executing).
* Add `std:Iter:no-errors` to check for error values in an iterator.
* Add `std:Unset:map` to easily map functions on Unset values.
* Add a `bind` module with `and` and `or` functions, to help with binding
  multiple cases.
* Add `std:type:Or` to create types which can be multiple other types.

#### Improvements
* `std:match` now takes an array of cases rather than using the remaining
  arguments as the cases.
  * It was basically always used with a merged array anyway.
* `std:log` has an interface like other modules, using indexing for `sublog`,
  `debug`, `info`, `warn`, and `error` rather than taking them as the first
  argument to the command.
* All functions have new implementations taking into account the new runtime
  semantics. In particular, when they are applied to arguments they will execute
  the function immediately rather than returning a value which will execute the
  function as before.
* Add `std:Error` to handle Errors as first-class types. Allow construction of
  errors with an explicit source location with the optional `source` keyed
  argument.
* Add a `priority` keyed argument to `task`, which allows setting a priority
  between 1 and 1000 for a task.
* Updated numeric arguments of functions to take the new `Number` type.
* `std:net:http` will use the system native certificates rather than the mozilla
  webpki set.
* `std:fs:copy` now supports a `shallow` keyed argument, which may be used to
  make shallow copies rather than actual file copies.

#### Breaking Changes
* Renamed the `task-count` keyed argument of `task` to `count`.
* Renamed `Iter:map-all` to `Iter:map` (that should be preferred), and `Iter:map`
  to `Iter:map-lazy`.
* Reordered `std:fs:unarchive` parameters to make more convenient to use with
  pipe operators.
* Changed `std:env:path-search` to return `Unset` rather than `Error` if no
  binary is found.
* `std:fs:copy` now defaults to making actual copies rather than hard links.
* Reordered `std:meta:{get,set}` parameters to put the value last, aligning
  better with other function signatures.
* Removed the `std:Unset:` function in favor of the builtin `:unset`.
* Rename `std:net:unarchive-remote` to `std:net:unarchive`.

#### Migration Guide
* `std:match` cases should be passed as an array.
* `std:log` should be accessed with an index operation.
* Language semantics have changed, so the use of value-level functions (like
  `std:dynamic`, `std:typed`, etc) should be examined.
* `std:seq` should be replaced with a syntax block.
* `std:error:throw` should be replaced with `std:Error:`.
* `std:error:catch` should be replaced with `std:match` with a `std:Error` case.
* `std:script` functions should be replaced with different means to get path
  information.
* Use `std:if` in place of old `if` expressions. If the `if` expression was
  using a binding, use `std:match`.
* Use `count` rather than `task-count` in keyed arguments to `task`.
* `std:Iter:map` should be replaced with `std:Iter:map-lazy`, and
  `std:Iter:map-all` should be replaced with `std:Iter:map`.
  * You should also review whether you really want `map-lazy`; in general (if
    you will eventually use all values) `map` is preferred.
* Flip arguments to `std:fs:unarchive`.

## 1.0.0-beta.9.1  -- 2021-04-05
### Language/Runtime
#### Improvements
* Evaluate scripts concurrently to avoid stack overflows.
* More efficiently calculate type/trait uuids at runtime.

### Standard Library
* No changes!

## 1.0.0-beta.9  -- 2021-03-09
### Language/Runtime
#### New Features
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

#### Improvements
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

#### Bugfixes
* Fix incorrect parsing of indexing in e.g. `a |>:b:c`.
* Fix incorrect parsing of trailing colon in e.g. `a b:c:`.

#### Migration Guide
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

### Standard Library
#### New Features
* The standard library has been restructured into a more consistent hierarchy
  (see the documentation or the migration guide for details).
* Add `type:enum` and `type:struct` for common cases.
* Add `Map:Values`, `Map:Of`, `Array:Values`, and `Array:Of` to check the
  contents of arrays and maps.
* Add `net:unarchive-remote` function for fetching and caching a remote archive,
  and `ergo-remote` convenience function for loading a remote archive.
* Add a number of support features for interacting with `Unset` types:
  * The `Unset` function to match the type.
  * `optional` and `required` predicates, allowing/disallowing `Unset` in
    patterns.
  * `default` to specify default values for `Unset` values.
* Add `import` to easily use a map of keys to bind to indexed values.
* Add `Function:pass` convenience function, namely for use in `match` cases.
* Add `Function:recursive` to create recursive functions.
* Add `Function:partial` to partially apply functions.
* Add `Function:memoize` to cache function results based on arguments.
* Add `match:value` convenience function.
* Add `Path:with-output` to make output paths from commands more ergonomic.
* Add `String:join` to join iterators of strings.
* Add `MapEntry` type for iterators of `Map`.
* `type:new` accepts a `bind` keyword argument to dictate how value instances
  can bind. If unspecified, uses the `bind` behavior of whatever inner type is
  returned by the compose function.
* Add `Bool` type as well as `Bool:true`, `Bool:false`, and `Bool:from` to
  create and convert bool values.
* Add `fs:append` for appending to a file.
* Add `env:user-cache` and `env:system-cache` to get the user and system cache
  paths.
* Add `env:os` to get the OS running ergo.
* Add `Iter` type for iterators, with the following support functions:
  * `from` to convert values to iterators,
  * `fold` to fold an iterator into a value,
  * `filter` to filter values in an iterator,
  * `flatten` to flatten an iterator of iterators,
  * `map` to map values in an iterator,
  * `map-all` to concurrently map values in an iterator,
  * `skip` to skip a number of values in an iterator,
  * `skip-while` to skip values based on a predicate in an iterator,
  * `split` to split an iterator into two after a number of values,
  * `take` to take a number of  values in an iterator,
  * `take-while` to take values based on a predicate in an iterator,
  * `unique` to deduplicate values in an iterator,
  * `zip` to zip/unzip iterators into array iterators.
* Add `Array:from` and `Map:from` (mainly for conversion from iterators).
* Add `typed` function to evaluate a value to a typed value.

#### Improvements
* Allow `env:path-search` to take Paths, and simply forward them as the returned
  value for convenience.
* Make `value:cache` also serve as a runtime cache, and add a `no-persist`
  option to make it _only_ serve as a runtime cache.
* Add `pattern` option to `error:throw` to indicate the error is a pattern
  error (which may be ignored by something like `match`).
* `value:meta:get` and `env:get` return `Unset` rather than `()` if a return is
  not available.
* Make `type:new` produce a value that behaves exactly like all other type
  functions for consistency (calls check types, call with no args returns a
  function to compose types).
* Allow `value:cache` to drop values early when they are present in the cache.
  This mainly makes things like a `task` value not count toward the pending
  work.
* Remove `collection` in favor of iterators and array-/map-specific functions.

#### Bugfixes
* Fix an issue where `match` incorrectly detected bind errors when a binding in
  the body of a case failed.
* Fix a panic when `script:path` is called when no path is available (now it will
  be a runtime error).

#### Migration Guide
* Values produced by `type:new` (custom script types) will now behave like other
  types in the standard library, meaning that calling the produced types merely
  checks the type (without (de)composing it as the previous behavior was). This
  means that anywhere where these types are used must be changed from
  ```
  MyType :x :y :z
  ```
  to
  ```
  MyType: :x :y :z
  ```
  where calling the type with no arguments returns the compose/decompose
  function. Since previously this was always the behavior, it will be sufficient
  (though not using the new features here) to change all calls of the custom
  types by adding a trailing colon as above.
* Anything previously using functions in `collection` will have to be convert to
  iterator functions:
  * `collection:map :f :v` -> `Array:from <| Iter:map :f :v`
  * `collection:fold :f :orig :v` -> `Iter:fold :f :orig :v`
  * `collection:entries :v` -> `Iter:from :v | Iter:map (fn :entry -> {key = entry:key, value = entry:value}) | Array:from`
  * `collection:get` and `collection:has` are no longer necessary (use `Unset` return type checking).
  Note that the above are directly equivalent conversions, but with the new
  iterator functions you can probably have better functionality not constrained
  as before.
* Functions/modules in the `value` module have been moved to the top level.
* `value:doc` has been removed as the builtin `doc` is sufficient.
* Type-related functions are now indexed off the type checking function:
  * Values in the `string` module are now accessed with `String`.
  * Values in the `path` module are now accessed with `Path`.

## 1.0.0-beta.8  -- 2021-01-15
### Language/Runtime
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

#### Migration Guide
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

### Standard Library
* Fix deadlock occurring from task execution.
  * Deadlocks are no longer possible, however other functions must indicate
    whether they want a task to be considered active across specific await
    points.
* Add a bunch of new functions:
  * `script:dir` to get the currently executing script's directory
  * `script:path` to get the currently executing script's path
  * `script:load-path` to get the current load-path
  * `path:name` to get the final component of a path
  * `value:dynamic` to create an explicitly-dynamic value
  * `match` to replace the previously parsed match expression
* Add the `type` module, with the following functions:
  * `new` to create new types from an interface definition
  * `Any`/`Unit`/`String`/`Path`/`Map`/`Array`/`Function` to match each type

## 1.0.0-beta.7  -- 2020-12-08
### Language/Runtime
* Add `-e`/`--expression` flags to evaluate the arguments as an expression
  without preceding `ergo`.
* Fix evaluation when pipe operators are present in the command-line arguments.
* Change the first `:` to `|>:` in command-line arguments for convenience.

### Standard Library
* No changes!

## 1.0.0-beta.6  -- 2020-12-07
### Language/Runtime
* Fix log pausing to correctly clear the rendered content.
* Fix mac plugin detection.

### Standard Library
* Fix unit-type returns (using new runtime unit type).
* Improve task work tracking; previously it only started tracking after the task
  is started.

## 1.0.0-beta.5  -- 2020-12-07
### Language/Runtime
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

### Standard Library
* Add `task-count` keyword argument to `task`, allowing one to specify how many
  task slots the task should consume when running.
* Track work of active tasks (displayed in progress statistics in log output).
* Add `value:doc:{get,set}` and `value:meta:{get,set}` for getting and setting
  documentation and metadata on values.

## 1.0.0-beta.4  -- 2020-11-17
### Language/Runtime
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

### Standard Library
* Rename `value:force` to `value:by-content`.
* Add `collection:get` to index values without causing errors to occur.
  * If a key/index doesn't exist, the function returns `()`.
* Remove `if` function as it is now a syntactic operator.
* Add `script` module and `script:bindings` function to return a map of all
  current bindings.
* Add `script:set-load-path` (previously one could set `load-path` in the
  environment).
* Improve `fs:copy` to create parent destination directories if missing.

## 1.0.0-beta.3  -- 2020-11-4
### Language/Runtime
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

### Standard Library
* Improve byte streams to support async io, allowing `io:stdin`, `io:stdout`,
  and `io:stderr` to be implemented.
* Change `exec` to produce asynchronous byte streams, allowing interactive
  processes to be run with concurrent IO.
* Update `if`, `collection:fold`, and `error:catch` to produce dynamically-typed
  values. This fixes a critical soundness bug that previously existed in
  `collection:fold`, assuming the returned value was the same type as the base
  value without checking it.
* Add `collection:has` to check whether a map/array has a given index, which
  complements indexing being changed to error when an index is missing.
  * This is only possible now that index expressions aren't evaluated
    immediately, such that `if` with an index operation will not evaluate until
    after the condition is first checked.

## 1.0.0-beta.2  -- 2020-10-07
### Language/Runtime
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

### Standard Library
* Add `fs:read`, `fs:write`, `fs:remove`.
* Rename `fs:mount` as `fs:unarchive`.
* Change `path` module to be a real module (rather than a function hack as it
  was before). This is possible since we can now call functions with no
  arguments (for `path:new`).
* Add `env:current-dir`.
* Statically link liblzma, use rustls-tls to remove dynamic linking of ssl
  libraries.

## 1.0.0-beta.1  -- 2020-09-24
### Language/Runtime
* Change to tokio runtime.

### Standard Library
* Fix numerous bugs encountered in standard library.

## 1.0.0-beta.0  -- 2020-09-21
* Initial release.
