# Macros
Macros are likely too complicated a use case for the scripting language. They
would lend expressive power to the language, but it might be wasted effort
overall. The primary additions to the language necessary to facilitate them
would be commands to both construct and destruct expressions (if we want
expression-level macros). We *could* go lower and have commands for
constructing/destructing tokens, but that would likely be too fine-grained.

```sh
match = macro [value,patterns] {
    try_pattern = \p -> expr match p {
        nil => if (eq $value ()) t ()
        string s => if (eq $s $value) t ()
        array vals => if (isarray $value) (map $match $value $vals) ()
        block exprs => if (ismap $value) (map $match $value $exprs) ()
        command v _ => {$v=$value}
    }

    expr match $patterns {
        block exprs => map $try_pattern $exprs
    }
}
```
