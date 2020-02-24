# Merge
A merge operation could be a command, but it is likely best as an operator that
cannot be a command (it is applied at parse-time rather than runtime), as this
will always make it obvious when/where a merge is occurring.

There are three cases:
1. A merge of a map in a block will expose all values of the map in the block's
environment:
```sh
{
  a = 1
  b = 2
  ^{c=3,d=4}
  e=$c
}
```
evaluates to a map with `{a=1,b=2,c=3,d=4,e=3}`.

2. A merge of an array in an array will concatenate all array values into the
outer array in-place:
```sh
[a,b,^[c,d],e]
```
evaluates to an array with `[a,b,c,d,e]`.

3. A merge of an array or a map in a command will either add the array values
as positional arguments in-place, or the map key/values as non-positional
arguments:
```sh
exec command arg1 ^[arg2,arg3] ^{env={PATH=/usr/bin}} arg4
```
Note that this is the only way to add non-positional arguments to a command.

