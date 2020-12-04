# ergo changelog

## 1.0.0-beta.5  -- Unreleased
* Improve task tracking and UI display of running tasks.
* Change the application log to be written to the data local directory. Support
  `ERGO_LOG_FILE` environment variable to override this.
* Improve error logging to accurately track pending errors.
* Add `-d`/`--doc` flags for getting documentation for the final value.

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
