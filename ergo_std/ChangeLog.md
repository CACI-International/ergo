# ergo_std changelog

## Unreleased
### New Features
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

### Improvements
* Improve error return values when using `std:eval`.
* Do not cache `std:net:unarchive` results, to make the standard library purely
  functional.
* Allow all `std:net:http` arguments to be used with `std:ergo-remote`.
* Allow `std:fs:copy` to copy symlinks rather than follow them by default.
  * Add a `follow-symlinks` keyed argument to change this behavior.
* Improve `std:Path:or-source` to error when non-Path values are passed.
* Change `std:doc:functions` to generate per-function doc children.
* No-arg functions are changed to take a single `()` argument to be idiomatic.
* Change type functions to use the `@` index for binding
  construction/destruction.
* Change `std:Any` to be more useful. It will evaluate the value and reject any
  Errors.
* Add `else` keyword and `else if` additional cases to `std:if`.
* Change `std:match` to reject Errors by default, and add an `allow-error` flag
  to allow errors (matching the old behavior).

### Bugfixes
* Fix the `std:fs:glob` relative directory.
  * It was mistakenly using the call site script path rather than the parent
    directory.
* Fix the behavior of `std:require` when a value is Unset, and ensure that
  documentation is carried over through `std:import` uses.
* Correctly capture network client errors in the runtime.
* `std:String:format` patterns would match string prefixes rather than whole
  strings; this has been fixed.

### Migration Guide
* Change the following functions from `function:` to `function ()`:
  * `std:Path:new`
  * `std:io:stdin`
* Change all type construction/destruction to use `Type:@` rather than `Type:`.
  * This includes _all_ types in `std` as well as any types created with
    `std:type` functions.
* If `std:Any` was being used, ensure its usage aligns with the new semantics.
  * A value matching the old semantics would be:
  ```
  OldAny = std:match:value [fn :x -> :x, pat :tgt -> :x -> bind :tgt :x, :x -> :x]
  ```
* Add an `else` keyword to `std:if` calls when necessary.
* Add `^allow-error` to `std:match` calls where matching errors is intentional
  (there probably aren't many cases of this).

## 1.0.0-rc.0  -- 2021-08-01
### New Features
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

### Improvements
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

### Breaking Changes
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

### Migration Guide
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

## 1.0.0-beta.9  -- 2021-03-09
### New Features
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

### Improvements
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

### Bugfixes
* Fix an issue where `match` incorrectly detected bind errors when a binding in
  the body of a case failed.
* Fix a panic when `script:path` is called when no path is available (now it will
  be a runtime error).

### Migration Guide
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

## 1.0.0-beta.8  -- 2020-01-15
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
* No changes!

## 1.0.0-beta.6  -- 2020-12-07
* Fix unit-type returns (using new runtime unit type).
* Improve task work tracking; previously it only started tracking after the task
  is started.

## 1.0.0-beta.5  -- 2020-12-07
* Add `task-count` keyword argument to `task`, allowing one to specify how many
  task slots the task should consume when running.
* Track work of active tasks (displayed in progress statistics in log output).
* Add `value:doc:{get,set}` and `value:meta:{get,set}` for getting and setting
  documentation and metadata on values.

## 1.0.0-beta.4  -- 2020-11-17
* Rename `value:force` to `value:by-content`.
* Add `collection:get` to index values without causing errors to occur.
  * If a key/index doesn't exist, the function returns `()`.
* Remove `if` function as it is now a syntactic operator.
* Add `script` module and `script:bindings` function to return a map of all
  current bindings.
* Add `script:set-load-path` (previously one could set `load-path` in the
  environment).
* Improve `fs:copy` to create parent destination directories if missing.

## 1.0.0-beta.3  -- 2020-11-04
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
* Add `fs:read`, `fs:write`, `fs:remove`.
* Rename `fs:mount` as `fs:unarchive`.
* Change `path` module to be a real module (rather than a function hack as it
  was before). This is possible since we can now call functions with no
  arguments (for `path:new`).
* Add `env:current-dir`.
* Statically link liblzma, use rustls-tls to remove dynamic linking of ssl
  libraries.

## 1.0.0-beta.1  -- 2020-09-24
* Fix numerous bugs encountered in standard library.

## 1.0.0-beta.0  -- 2020-09-21
* Initial release.
