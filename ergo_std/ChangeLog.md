# ergo_std changelog

## Unreleased
### New Features
* Add `enum`, `struct`, `MapValues`, `MapOf`, `ArrayValues`, and `ArrayOf` to
  `type` module.
* Add `net:unarchive-remote` function for fetching and caching a remote archive,
  and `ergo-remote` convenience function for loading a remote archive.
* Add `type:Unset` function to match `Unset` types.
* Add `type:Optional` and `type:Required` predicates, checking for `Unset`.
* Add `default` to specify default values for `Unset` values.
* Add `import` to easily use a map of keys to bind to indexed values.
* Add `pass` convenience function, namely for use in `match` cases.
* Add `recursive` to create recursive functions.
* Add `match:value` convenience function.
* Add `path:with-output` to make output paths from commands more ergonomic.
* Add `Iter` type for iterators.
* Add `string:join` to join iterators of strings.
* Add `MapEntry` type for iterators of `Map`.
* `type:new` accepts a `bind` keyword argument to dictate how value instances
  can bind. If unspecified, uses the `bind` behavior of whatever inner type is
  returned by the compose function.
* Add `type:Bool` as well as `bool:true`, `bool:false`, and `bool:from` to
  create and convert values.
* Add `fs:append` for appending to a file.
* Add `env:user-cache` and `env:system-cache` to get the user and system cache
  paths.

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
