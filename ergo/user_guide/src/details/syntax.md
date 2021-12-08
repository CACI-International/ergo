# Syntax and Semantics

This guide describes the syntax of ergo scripts. It starts with the basics of
parsing and builds on them incrementally. Ergo scripts are parsed in four
stages; the first 2 are tokenization and tree tokenization (to match paired
tokens), which occur serially (without loading everything into memory at once).
The next 2 stages are tree parsing and expression parsing, which do load all
tree-tokens into memory and recursively parse the structures.

## Tokenization
Script tokenization is contextual, producing a stream of tokens which can be
used to build a syntax tree. The resulting stream of tokens contains no
whitespace. It occurs according to the following rules:

Within a normal context (e.g. not a string):
* Symbolic tokens are always read as such (`-> : ! = ^ | |> <|`).
* Paired tokens (`{ } [ ] ( )`) are read and verified to be correctly paired. 
* Sequences of non-newline whitespace are read as a single whitespace token
  (additional whitespace does not matter to parsing).
* Newlines, commas, and semicolons are read as such and emit child separator
  tokens when appropriate (within curly brackets and square brackets).
* A `#` (no following space) will be read as a tree comment token.
* A `# ` will cause any following characters until a newline or end of
  stream to be ignored (line comments).
* A `##` (no following space) will be read as an attribute token.
* `## ` or `' ` will be read in a contiguous block over subsequent lines and
  emit virtual paired tokens at the beginning and end. Everything from the
  leading characters to the newline is tokenized as a quoted string context.
* If a `"` is read, all characters (including whitespace, newlines, etc) until
  the matching `"` are read as a string. A `\` may be used to escape the
  following characters:
  * `"`: double quote character
  * `\`: backslash character
  * `n`: newline character
  * `t`: tab character
  * `\u{xxxxxx}`: an arbitrary unicode character, with 1-6 hex digits
    identifying the code point
  The interior of the `"` characters is tokenized as a quoted string context.
* Any sequences of characters that don't match the above are read as a string.

Within a quoted string context:
* A `^` followed by a paired token will read until the matching paired token
  using a normal tokenizing context.
* A `^` followed by `[-_A-Za-z0-9]+` will read the single word using a normal
  tokenizing context.
* A `^` folowed by a `^` will read as if a single `^` string were in that
  location (to insert a literal `^`).
* Any sequences of characters (including whitespace) that don't match the above
  are read as a string.

## Tree Parsing
Tree parsing ingests the tokens from the previous step and builds a parsed tree
of items, where the pipe operators (`|`, `|>`, and `<|`) are desugared. It also
disambiguates infix operators using rules of precedence, where the precedence of
operators is as follows (descending):
* `:` (prefix)
* `:` (left associative)
* `!`/`^` (prefix)
* `!` (prefix of an entire expression)
* `->` (right associative)
* `|`/`|>` (left associative)
* `<|` (right associative)
* `=` (left associative)
* `^` (prefix of an entire expression)


### Syntax Sugar

#### Pattern expression strings
If a pattern expression to the left of a `=` is only a string, this step elaborates this to a set expression:
```ergo
a = 1
```
is the same as
```ergo
:a = 1
```
This is generally just a convenient behavior for the common case of creating a
single binding.

#### Unquoted block strings
Tree parsing elaborates an unquoted string literal within curly brackets to a
set/get binding with that string (effectively setting an existing binding within
the block):
```ergo
{a}
```
is the same as
```ergo
{:a = :a}
```

#### Pipe operators
Pipe operators desugar in the following ways:
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

#### Index/Command strings
If a string is the value to index or first child of a command or pattern
command, it will desugar to a get expression:
```ergo
f a b c
map:ind
g :z = 1
```
is the same as
```ergo
:f a b c
:map:ind
(!:g) :z = 1
```

The resulting AST tree is composed of operations, strings, doc comments, and
groupings.

#### String expressions
If a string is the lone value being merged into a quoted string, it will desugar
to a get expression:
```ergo
"hello ^name"
## Arguments: ^(args)
```
is the same as
```ergo
"hello ^(:name)"
## Arguments: ^(:args)
```

## Expression Parsing
Finally, the parsed trees from the previous step are parsed into script
expressions. This step:
* ensures `^` is applied in valid expressions (within
  blocks/arrays/commands),
* ensures `=` is applied in valid expressions (within blocks/commands), and
* removes any tree-commented (`#`) expressions, and
* parses the colon operators, the string `_`, and commands differently based on
  whether the expression is a pattern expression (left of a `=` or `->`) or a
  normal expression.

Below are examples of the valid expressions in the language:

### Unit
```ergo
()
```
Evalutaes to a unit value.

### Bind Any
```ergo
_ = something
```
In a pattern expression, the unquoted string `_` evaluates to a value which can
be successfully bound with any value.

### String
```ergo
hello
this-is-a-string
"this is a string"
"quote:symbols"
"quote\nescaped\t\"things\""
' multiline strings
' are combined together
```
Strings evaluates to literal string values.

### Compound String
```ergo
"compound ^string"
' ^(doc :value)
' some additional info over
' multiple lines
```
Compound strings are any quoted strings with at least one `^` expression within.
The value parsed by the `^` will be displayed in the string at that location.

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

### Block
```ergo
{}
{a = 1, b = 2}
{
  a = 1
  ()
  b = 2
}
{a = 1; b}
{a = 1; "b"}
```
Blocks are surrounded with curly brackets, and items are separated by comma,
semicolon, or newline. Blocks deeply evaluate each expression/binding in
sequence. Blocks create a new binding scope. If the last expression is a
binding, the block evaluates to a Map with entries of all bindings introduced in
the block scope. If not, a block evaluates to the final expression. Binding keys
are not constrained to strings, they can be _any_ value.

### Function
```ergo
_ -> b
f = fn :a :b -> [:a,:b]
```
Functions are created with the `->` operator. Note that functions are so-named
for familiarity, but are actually _unbound_ values which may be bound later. The
`fn` function (yes, it is itself a builtin function) allows you to match command
arguments in a binding, but you can also create unbound values (like the above
`_ -> b`) which can be used in other contexts aside from just commands.

The left side of a `->` operator is parsed as a pattern expression.

A function expression evaluates to a value which, when bound, will bind to the
left pattern expression and (if successful) return the right expression with the
captured values.

### Bind Statement
```ergo
a = 1
:b = 1
```
Bind statements are created with the `=` operator.

The left side of a `=` operator is parsed as a pattern expression.

Bind statements are only valid within blocks and commands.

### Get
```ergo
:a
:[a,b]
```
To get a value from the enclosing lexical binding scope, you precede the key
with `:`. This is _only_ in normal expressions, in pattern expressions a `:`
evaluates to a set expression (see below). A get expression captures the bound
value, or an error occurs if no such binding exists.

### Set
```ergo
:a = b
:a -> b
:[a,b] = 1
```
In a pattern expression (the left side of `=` and `->`), a colon preceding an
expression will cause the expression to be evaluated as a _normal_ expression
and used as a key to create a binding in the current scope. A set expression
evaluates to an unbound value which, when bound, will populate the set value in
all expressions that have captured the binding with a get expression.

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
The expression evaluates to the result of indexing. 

*Sugar*: Index expressions will evaluate as if they were written as `!a:b` (see
the `Force` operator) if the left side is a captured expression.

The `Array` and `Map` types support indexing to retrieve the contents. For maps,
index with the map key. If the key does not exist, the expression will evaluate
to an `Unset` value. For arrays, index with a 0-indexed number, or a negative
number to index from the end; if the index does not exist an error will occur. 

### Command
```ergo
std:exec ls -l
:my-fn a b c
my-fn (kwarg=123) a b c
f a b (c d e)
```
A command expression is any group in a normal expression (where parentheses can
be used to create groups) that has more than one child. If you need to create or
call a command with no arguments, technically you can use `f ^[]`, however
idiomatically functions should take a single `()` argument (e.g. `f ()`). A
command expression will evaluate the first child and bind it to an `Args`-typed
value that contains the remaining children as positional arguments. Any bind
statements or map merge statements will set the keyed arguments in `Args`.

The expression evaluates to the result of binding the first child with the
`Args` value. One can match such a binding with the builtin `fn` function:
```ergo
pair = fn :x :y (extra = :z) -> std:if :z [:x,:y,:z] else [:x,:y]
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
call with no arguments.

The difference is that instead of binding an `Args`-typed value, it
binds a `PatternArgs`-typed value to differentiate the case. One can match such a
binding with the builtin `pat` function:
```ergo
unpair = pat :x :y (extra = :z) -> :value -> {
    bind :x value:0
    bind :y value:1
    std:if :z (bind :z value:2)
}
unpair :a :b = [1,2] # 'a' now evaluates to 1, 'b' now evaluates to 2
unpair (extra=:c) :a :b = [1,2,3] # a => 1, b => 2, c => 3
unpair (unpair :a :b) (unpair :c :d) = [[1,2],[3,4]] # a => 1, b => 2, c => 3, d => 4
```

Note that `pat` returns a value that will be applied in the pattern expression,
so typically you want the right hand side to evaluate to a function/unbound
value that will then be bound to whatever value is bound subsequently (e.g.,
`:value` in the above is bound to the right-hand side of the `=` statements).

### Force Expression
```ergo
!std:string:format "{}" :a
std:string:format "{}" !(f :a)
```
A force expression uses a prefix `!` to force the following expression to be
evaluated and captured as soon as possible. _As soon as possible_ means as soon
as all nested captures are resolved/bound.

The prefix may be at the very start of a grouping (e.g. as it is in the second
case above) or directly before a group item expression (as in the third case).

The expression is actually applied prior to any other expression at a syntactic
level rather than during runtime evaluation.

### Merge Statement
```ergo
a = [1,2]
b = [^:a,3,4] # same as [1,2,3,4]

^:map
m = {a = 1, b = 2}
k = {a = 5, ^:m, b = 3, c = 4} # same as {a = 1, b = 3, c = 4}

command = [ls,-l]
exec ^{pwd = /home} ^:command
```
A merge statement uses a prefix `^` to indicate that a value should be merged
into the enclosing array/block/command expression.

In arrays, you may merge other arrays, and the values of merged arrays will be
concatenated in-place.

In blocks, you may merge a map, and all keys in the map will become bindings in
the block. You may merge an array to sequentially insert the values of the array
as expressions in the block. You may merge a string to insert that string as a
key with a unit value.

In command and pattern command expressions, in any position following the first
child a map (for keyed arguments), array (for positional arguments), string (for
a keyed argument with no value, useful for flags) or Args/PatternArgs can be
merged. 

In quoted strings, you can merge any value which can be displayed.

### Doc Comments
```ergo
## Provide a friendly greeting.
hello = fn :name -> "Hello, ^name!"

## ^(doc :hello) But not too friendly.
warn-of-fire = fn :name -> "^(hello :name) Your pants are on fire."

## A map containing:
## ^(doc (doc:value()):something)
map = {
    something = my-value
}
```
Doc comments are identified by two `#` characters followed by a space. Doc
comments may only be present when unambiguous: in groups where newlines separate
items (i.e. blocks and arrays) and within parentheses prior to exactly one
expression. Multiple lines of doc comments are merged together into a single
comment on the first non-doc-comment expression that follows them.

Doc comments may contain `^` just like quoted strings, which will evaluate the
value and display it at that location within the string.

### Attributes
```ergo
##std:doc:module
value = a b c
```
Attributes may precede values in the same way that doc comments do (and can be
combined with doc comments and other attributes). They simply evaluate the value
of the attribute and then bind the value to which they are applied to that
result, returning the result of binding. They are most useful and recommended
for making changes to metadata, where the changes don't affect the immediate
functionality of the code.
