#!/bin/sh

# This script bootstraps an ergo installation into the 'dist' folder in the current directory.

OUTPUT=dist

set -e

DIR=$(dirname $0)

TYPE=debug

if [[ " $* "  == *\ --release\ * ]]; then
	TYPE=release
fi

(cd $DIR; cargo build -p ergo -p ergo_std "$@")

DYEXT=so
if [[ "$OSTYPE" == "darwin"* ]]; then
	DYEXT=dylib
fi

rm -fr $OUTPUT

mkdir -p $OUTPUT/bin
mkdir -p $OUTPUT/share/ergo/lib
cp $DIR/target/$TYPE/ergo $OUTPUT/bin/ergo
cp -r $DIR/ergo_std/script $OUTPUT/share/ergo/lib/std
cp $DIR/target/$TYPE/libergo_std.$DYEXT $OUTPUT/share/ergo/lib/std/plugin.ergo
