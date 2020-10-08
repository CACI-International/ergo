# ergo changelog

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
