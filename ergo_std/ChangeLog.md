# ergo_std changelog

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
