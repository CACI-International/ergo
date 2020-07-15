# Getting Started

Let's start with a common workflow that is typically automated with tools like
GNU `make`: compiling C/C++ source code. This tutorial will build from a simple,
one-command build step to a concurrent build including unit tests and release
deployment. While `so` may support plugins to perform these tasks in even more
consistent and cross-platform ways, this tutorial will stick with only using
external commands for the tasks, and assumes a unix-like environment.

Before we begin, let's cover some nomenclature.

A __script__ is a file conforming to the expected `so` syntax, which describes a
dependency tree of commands.

A __command__ is a single logical operation in a script. It may involve running
an external binary, running a built-in function, scheduling tasks on remote
resources, etc.

__Data__ is the term used for script runtime information; it can take a number
of common forms, which will be described later.

A __future__ is a _lazily-evaluated_ piece of data in the script runtime. While
the future itself is lazily-evaluted, it has attached metadata such as type
information.
