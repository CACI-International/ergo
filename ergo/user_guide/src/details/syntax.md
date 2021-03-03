# Syntax and Semantics

This guide describes the syntax of ergo scripts. It starts with the basics of
parsing and builds on them incrementally. Ergo scripts are parsed in four
stages; the first 2 are tokenization and tree tokenization (to match paired
tokens), which occur serially (without loading everything into memory at once).
The next 2 stages are tree parsing and expression parsing, which do load all
tree-tokens into memory and recursively parse the structures.

## Tokenization
Script tokenization is very straighforward, and occurs according to the
following rules:
* A single `#` will cause any following characters until a newline or end of
  stream to be ignored (comments).
* Two `##` will parse the remaining characters as a doc comment.
  * Within doc comments, `{{` and `}}` are parsed as doc comment expression
    tokens, and within them any tokens are parsed as below.
* Symbolic tokens are always parsed as such (`-> { } [ ] ( ) : ! = ^ | |> <| ,
  ;`), unless in a quoted string.
* Sequences of non-newline whitespace are parsed as a single whitespace token
  (additional whitespace does not matter to parsing).
* Newlines are parsed as such.
* If a `"` is read, all characters (including whitespace, newlines, etc) until
  another `"` are parsed into a string. A `\` may be used to escape the
  following characters:
  * `"`: double quote character
  * `\`: backslash character
  * `n`: newline character
  * `t`: tab character
* Any sequences of characters that don't match the above are parsed as a string.

## Tree Tokenization
Tree tokenization iterates over the tokens from the previous step, ensuring that
all paired tokens are matched and emitting a token stream of events, including
the normal symbolic tokens and strings, start/end nested groups, next child for
groups, and colons disambiguated based on whitespace.

This tokenization step removes all whitespace, semicolons, and commas, as once
the tree is tokenized these are no longer needed to disambiguate parsing.

Note that children of curly brackets and square brackets are separated by
semicolon, comma, or newline, whereas children of parentheses are separate by
whitespace and newlines.

## Tree Parsing
Tree parsing ingests the tree tokens from the previous step and builds a parsed
tree of items, where the pipe operators (`|`, `|>`, and `<|`) are desugared. It
also disambiguates infix operators using rules of precedence, where the
precedence of operators is as follows (descending):
* `:` (prefix)
* `:` (left associative)
* `:` (suffix)
* `!`/`^` (prefix)
* `!` (prefix of an entire expression)
* `->` (right associative)
* `<|` (right associative)
* `|`/`|>` (left associative)
* `=` (left associative)
* `^` (prefix of an entire expression)

*Sugar*: This step elaborates a string literal within curly brackets to a
set/get binding with that string (effectively setting an existing binding within
the map/block):
```ergo
{a}
```
is the same as
```ergo
{:a = :a}
```

*Sugar*: Pipe operators desugar in the following ways:
* `a b c |> d e f` groups the left expressions, typically creating a command
  expression which is then called on the right expressions: `(a b c) d e f`,
* `a b c <| d e f` groups the right expressions, typically creating a command
  expression which is the last argument in addition to the left expressions in a
  command: `a b c (d e f)`,
* `a b c | d e f` groups the left expressions, and moves the group to be the
  last expression of the right expressions: `d e f (a b c)`.
These operators can be used in arbitrary positions and whitespace importance
(for instance for `:` tokens) is preserved:
```ergo
a b c |>:d # same as (a b c):d
a b c |> :d # same as (a b c) :d
a b |>:<| c d # same as (a b):(c d)
```

The resulting AST tree is composed of operations, strings, doc comments, and
groupings.

## Expression Parsing
Finally, the parsed trees from the previous step are parsed into script
expressions. This step:
* groups doc comments and applies them to appropriate values (the following
  expression, or the expression being bound if the following expression is a
  bind expression) and errors if a doc comment is in an invalid position, 
* ensures `^` is applied in valid expressions (within
  maps/blocks/arrays/commands),
* parses `if` as a special expression,
* parses disambiguates curly-bracket syntax to maps and blocks, and
* parses the colon operators, the string `_`, `=`, etc. differently based on
  whether the expression is a pattern expression (left of a `=` or `->`) or a
  regular expression.

Below are examples of the valid expressions in the language:

### Empty
```ergo
()
```
Evalutaes to a unit value.

### Bind Any
```ergo
_ = something
```
In a pattern expression, the string `_` evaluates to a value which can be
successfully bound with any value.

### String
```ergo
hello
this-is-a-string
"this is a string"
"quote:symbols"
"quote\nescaped\t\"things\""
```
Strings evaluates to literal string values.

### Array
```ergo
[]
[a,b,c]
[a
 b
 c]
[a;b;c]
```
Arrays are surrounded with square brackets, and items are separated by comma,
semicolon, or newline. The above all evaluate to Array values containing each of
the separated items.

### Map
```ergo
{}
{a = 1, b = 2}
{
  a = 1
  ()
  b = 2
}
{a = 1; b}
```
Maps are surrounded with curly brackets, and items are separated by comma,
semicolon, or newline. Maps must have a literal bind expression as the last
expression, otherwise they are interpreted as block expressions (see below).
The above all evaluate to Map values, where each bound expression in scope makes
up the keys and values of the map. Note that you can have expressions that are
not bind expressions as items in a map, just the last expression must be a bind
expression. Each line in the map is evaluated in sequence.

Map keys are not constrained, they can be _any_ value.

### Block
```ergo
{
  a = 1
  ()
}
```
Blocks are surrounded with curly brackets, and items are separated by comma,
semicolon, or newline. Blocks must not have a literal bind expression as the last
expression, otherwise they are interpreted as map expressions (see above).

Blocks evaluate to the last value in the block. Each line of the block is
evaluated in sequence (though this doesn't necessarily mean the values are
forced).

### Function
```ergo
_ -> b
f = fn :a :b -> [:a,:b]
```
Functions are created with the `->` operator. Note that functions are so-named
for familiarity, but are actually _unbound_ values which may be bound later. The
`fn` function allows you to match command arguments in a binding, but you can also
create unbound values (like the above `_ -> b`) which can be used in other
contexts aside from just commands.

The left side of a `->` operator is parsed as a pattern expression.

A function expression evaluates to a value which, when bound, will bind to the
left pattern expression and (if successful) evaluate the right expression.

### Bind
```ergo
a = 1
:b = 1
```
Bind expressions are created with the `=` operator.

*Sugar*: if the left side of the `=` operator is a string literal (e.g. `a`
above), it is elaborated to be the same as if you had written a set expression
(e.g. `:a = 1`).

The left side of a `=` operator is parsed as a pattern expression.

A bind expression evaluates to a special type which should typically not be
used.

### Get
```ergo
:a
:[a,b]
```
To get a value from the current environment, you precede the key with `:`. This
is _only_ in normal expressions, in pattern expressions a `:` evaluates to a set
expression (see below). A get expression evaluates to the environment binding,
or errors if it is missing.

### Set
```ergo
:a = b
:a -> b
:[a,b] = 1
```
In a pattern expression (the left side of `=` and `->`), a colon preceding an
expression will cause the expression to be evaluates as a _normal_ expression
and used as a key in the current environment. A set expression evaluates to an
unbound value which, when bound, will set the value in the environment.

### Pattern Equal
```ergo
:a = :b = c
```
`=` is left associative, so the `:a = :b` in this expression is grouped. This is
a _pattern equal_ expression, which differs from a `bind` expression. A pattern
equal expression will evaluate to a value that, when bound, will try to bind to
both the left and right subexpressions. Thus, it allows you to bind a value more
than once. In the above example, both `a` and `b` in the environment are bound
to the string literal `c`.

### Index
```ergo
a:b
a:b:c
:a:b:c
a:b:c = [1,2]
```
The index operator `:` is an infix left-associative operator which will evaluate
both subexpressions and bind the left with an Index containing the right. One
can match such a binding with the builtin `index` function:
```ergo
ind-to-array = index :a -> [:a]
ind-to-array:b # Evaluates to [b]
```
The expression evaluates to the result of indexing. It will eagerly evaluate if
the left side is immediately available (is not a delayed value), otherwise it
will evaluate to a delayed value itself.

The `Array` and `Map` types support indexing to retrieve the contents. For maps,
index with the map key. If the key does not exist, the expression will evaluate
to an `Unset` value. For arrays, if the index does not exist an error will
occur.

*Sugar*: If the left subexpression is a string literal, it will be elaborated to
a get expression (e.g. `a:b` => `:a:b`).

### Command
```ergo
std:exec ls -l
:my-fn a b c
my-fn (kwarg=123) a b c
f a b (c d e)
```
A command expression is any group in a normal expression (where parentheses can
be used to create groups) that has more than one child. If you need to call a
command with no arguments, add a `:` suffix (e.g. `my-function:`). A command
expression will evaluate the first child and bind it to an `Args`-typed value
that contains the remaining children as positional arguments. Any bind
expressions or map merge expressions will set the non-positional (keyword)
arguments in `Args`.

*Sugar*: If the first child expression is a string literal, it will be
elaborated to a get expression (e.g. `f a b` => `:f a b`).

The expression evaluates to the result of binding the first child with the
`Args` value. One can match such a binding with the builtin `fn` function:
```ergo
pair = fn :x :y (extra = :z) -> if :z [:x,:y,:z] [:x,:y]
pair 1 2 # evaluates to [1,2]
pair (extra=3) 1 2 # evaluates to [1,2,3]
```

### Pattern Command
```ergo
f a b c = d
```
A pattern command expression is any group in a pattern expression (where
parentheses can be used to create groups) that has more than one child. This
behaves identically to command expressions, including the use of a `:` suffix to
call with no arguments and the syntax sugar for the first child.

The difference is that instead of binding an `Args`-typed value, it
binds a `PatternArgs`-typed value to differentiate the case. One can match such a
binding with the builtin `pat` function:
```ergo
unpair = pat :x :y (extra = :z) -> :value -> {
    !:x = value:0
    !:y = value:1
    if :z (!:z = value:2)
}
unpair :a :b = [1,2] # 'a' now evaluates to 1, 'b' now evaluates to 2
unpair (extra=:c) :a :b = [1,2,3] # a => 1, b => 2, c => 3
unpair (unpair :a :b) (unpair :c :d) = [[1,2],[3,4]] # a => 1, b => 2, c => 3, d => 4
```

Note that `pat` returns a value that will be applied in the pattern expression,
so typically you want the right hand side to evaluate to a function/unbound
value that will then be bound to whatever value is bound with the `=` operator.

### If
```ergo
if a b
if a b c
```
An if expression is identified by the `if` keyword. The first argument is the
condition, the second is the value to which to evaluate if the condition is
true, and the third (optional) value is the value to which to evaluate
otherwise. If the third value is omitted, an `Unset` value is used.

The expression evalutes to a dynamically-typed value that, when evaluated, will
evaluate and check the conditional value and evaluate to the true or false case.

### If Pattern
```ergo
if (unpair :a :b = [1,2]) [:a] no
```
An if pattern expression is idenfitied by the `if` keyword like an if
expression, but is distinguished by the condition being a bind expression. In
this case, if the binding succeeds the true expression is used (with any
introduced bindings in scope), otherwise the false expression is used.  Like a
normal if expression, the false case may be omitted (and an `Unset` value will
be used).

The expression immediately evaluates the binding and then evaluates to either
the true or false case.

### Force
```ergo
!:value
!std:string:format "{}" :a
std:string:format "{}" !:a
```
A force expression uses a prefix `!` to force the value once. If the value is
dynamically-typed, it will evaluate it to get the inner value. If the value is
statically typed, it will evaluate the value by content.

The prefix may be at the very start of a grouping (e.g. as it is in the second
case above) or directly before a group item expression.

The expression immediately evaluated to the result of forcing the argument.

### Merge
```ergo
a = [1,2]
b = [^:a,3,4] # same as [1,2,3,4]

^std:
m = {a = 1, b = 2}
k = {a = 5, ^:m, b = 3, c = 4} # same as {a = 1, b = 3, c = 4}

command = [ls,-l]
exec ^{pwd = /home} ^:command
```
A merge expression uses a prefix `^` to indicate that a value should be merged
into the outer expression.

In arrays, you may merge other arrays, and the values of merged arrays will be
concatenated in-place.

In blocks and maps, you may merge a map, and all keys in the map will become
bindings in the outer block/map.

In command and pattern command expressions, anywhere following the first child a
map or array can be merged. If an array is merged, the items in the array become
in-place positional arguments to the command, and if a map is merged the
keys/values in the map become non-positional arguments to the command (thus the
position of the map merge is irrelevant).

Merge expressions sometimes return a merge value as an implementation detail
(mainly related to using merge expressions in pattern commands), but otherwise
one should not treat the expressions as something that itself evaluates to a
value.

### Doc Comments
```ergo
## Provide a friendly greeting.
hello = fn :name -> std:String:format "Hello, {}!" :name

## {{doc :hello}} But not too friendly.
warn-of-fire = fn :name -> std:String:format "{} Your pants are on fire." <| hello :name

## A map containing:
## {{doc self:something}}
map = {
    something = my-value
}
```
Doc comments are identified by two `#` characters. Doc comments may only be
present in groups where newlines separate items (i.e. blocks, maps, and arrays;
_not_ simply parentheses). Multiple lines of doc comments are merged together
into a single comment on the first non-doc-comment expression that follows them.
The first line of a doc comment (if not empty) determines how much leading
whitespace is stripped from all subsequent lines.

Doc comments may contain `{{ ... }}` blocks, which will evaluate the contents as
a normal block expression. The result of the block will be displayed inline in
the doc comment when rendered. Within these blocks, the string `self` is bound
to the value being documented. These blocks share an environment scope across
the entire doc comment.

The expressions in a doc comment behave as if they were written in a function
body. That is, they are not executed until the doc comment is needed.
