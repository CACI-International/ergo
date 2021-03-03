# Getting Started

Let's start with a common workflow that is typically automated with tools like
GNU `make`: compiling C/C++ source code. This tutorial will build from a simple,
one-command build step to a concurrent build including unit tests and release
deployment. While `ergo` may support plugins to perform these tasks in even more
consistent and cross-platform ways, this tutorial will stick with only using
external commands for the tasks, and assumes a unix-like environment.

Before we begin, let's cover some nomenclature.

A __script__ is a file conforming to the expected `ergo` syntax, which describes a
dependency tree of commands.

A __value__ is a _lazily-evaluated_ result in the script runtime. While the
value itself is lazily-evaluted, it has attached metadata such as type
information which is immediately available.
