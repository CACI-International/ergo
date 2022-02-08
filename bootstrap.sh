#!/bin/sh

# This script bootstraps an ergo installation into the 'dist' folder in the current directory.

OUTPUT=dist

set -e

DIR=$(dirname $0)

TYPE=debug

if [[ " $* "  == *\ --release\ * ]]; then
	TYPE=release
fi

(cd $DIR; cargo build -p ergolang -p ergo_std "$@")

DYEXT=so
if [[ "$OSTYPE" == "darwin"* ]]; then
	DYEXT=dylib
fi

rm -fr $OUTPUT

mkdir -p $OUTPUT/bin
mkdir -p $OUTPUT/lib/ergo
cp $DIR/target/$TYPE/ergolang $OUTPUT/bin/ergolang
cp -r $DIR/ergo_std/script $OUTPUT/lib/ergo/std
cp $DIR/target/$TYPE/libergo_std.$DYEXT $OUTPUT/lib/ergo/std/plugin.ergo
