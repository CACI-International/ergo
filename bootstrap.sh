#!/bin/sh

# This script bootstraps an ergo installation into the 'ergo-dist' folder in the current directory.

set -e

DIR=$(dirname $0)

TYPE=release
FLAGS=--release

if [ "$1" == "debug" ]; then
	TYPE=debug
	FLAGS=
fi

(cd $DIR; cargo build -p ergo -p ergo_std $FLAGS)

DYEXT=so
if [ "$OSTYPE" == "darwin"* ]; then
	DYEXT=dylib
fi


mkdir -p ergo-dist/bin
mkdir -p ergo-dist/share/ergo/lib
cp $DIR/target/$TYPE/ergo ergo-dist/bin/ergo
cp $DIR/target/$TYPE/libergo_std.$DYEXT ergo-dist/share/ergo/lib/std.ergo
