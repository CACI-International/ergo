# Closure Environment
In using `so`, I encountered a bit of an issue with how closures interact with
the environment. In particular, the issue is demonstrated by:

```
c++ = fn ^args -> {
    exec c++ -std=c++17 ^$args
}

c++ -c -o { file = object_file } (track input.cpp)
```

Currently, closures store their environment at the definition site, and on
invocation they are run with only that environment available. This works fine
for reading from the environment, but in the case of writing to the environment
(as occurs with the `{ file = object_file }` `exec` argument), this ends up
writing to an environment that is discarded.

Environment bindings within functions shouldn't default to writing to
the call-site environment, as that would mean any environment bindings
(including temporary ones) would pollute the call-site environment.

There are a couple of solutions to this problem.
1. **Mutability** The way that environment bindings like those done by the
   `exec` command could change entirely. Instead of giving a name to bind in the
   environment, we could pass a special environment slot/reference to populate:

   ```
   object_file = env slot
   c++ -c -o { file = $object_file } (track input.cpp)
   ```
   
   This introduces some mild mutability semantics into the runtime. It does,
   however, make the script more approachable (by being more similar to other
   languages) and makes it very clear where the environment value is ending up.
   One might consider adding a way of assigning these values in user functions,
   like `object_file set value`.

2. **Return** `exec` (and other functions) could be stripped of their ability to
   insert things into the environment, such that they would have to incorporate
   these changes as part of their return value. This may be appropriate, as the
   fact that `exec` inserts things into the environment has confused users. This
   functionality was done to make exec commands more terse, and it seemed useful
   since it's very rare for a user to specify files/directories for an external
   command and not need to use them.

   With this approach (for example)

   ```
   exec wget https://github.com/catchorg/Catch2/archive/v2.11.1.tar.gz -O { file = tarball }
   exec mkdir { dir = unpack }
   catch_dir = (unpack Catch2-2.11.1)
   exec ^{
       pwd = (unpack .)
       creates = {
           sourcedir = (catch_dir .)
       }
   } tar -xzf $tarball 
   ```

   would become

   ```
   ^((exec wget https://github.com/catchorg/Catch2/archive/v2.11.1.tar.gz -O { file = tarball }) files)
   unpack = (exec mkdir { dir = unpack }) dirs unpack
   catch_dir = (unpack Catch2-2.11.1)
   exec ^{
       pwd = (unpack .)
       creates = {
           sourcedir = (catch_dir .)
       }
   } tar -xzf $tarball 
   ```

   Note the first and second line are just examples of (1) inserting all files
   into the environment and (2) selecting a single file/dir rather than all of
   them.

   Also note that with this change, expressions in blocks would only be
   useful if their return value were bound in the environment, otherwise (since
   everything is purely functional in the runtime) they would have no effect in
   the overall script.

3. **Env Access** User and builtin functions could be provided with the
   call-site environment explicitly. With this approach, functions may read from
   the call-site environment _and_ insert into it. This has some utility in
   writing functions that behave similar to particular varieties of macros in
   other languages, as functions can now have free variables in their body (so
   call-site context is relevant).

   Example:

   ```
   c++ = fn ^{call-env} ^args -> {
       exec ^{ call-env = $call-env } c++ -std=c++17 ^$args
   }
   
   c++ -c -o { file = object_file } (track input.cpp)
   ```

   Example with user functions:
   ```
   set_a = fn ^{call-env} val -> {
       call-env a = $val
   }

   set_a "it's a!"
   $a
   ```

   Since functions that manipulate the call-site environment are not very
   intuitive (and could be abused), it would be advised to use this feature
   sparingly and/or document thoroughly.
