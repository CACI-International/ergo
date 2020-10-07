#!/bin/sh

# This script bootstraps an ergo installation into the 'ergo-dist' folder in the current directory.

set -eu

DIR=$(dirname $0)

(cd $DIR; cargo build -p ergo -p ergo_std --release)

DYEXT=so
if [ "$OSTYPE" == "darwin"* ]; then
	DYEXT=dylib
fi

mkdir -p ergo-dist/bin
mkdir -p ergo-dist/share/ergo/lib
cp $DIR/target/release/ergo ergo-dist/bin/ergo
cp $DIR/target/release/libergo_std.$DYEXT ergo-dist/share/ergo/lib/std.ergo
