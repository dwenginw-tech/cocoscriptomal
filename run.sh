#!/bin/bash
# CocoScript build and run script for Linux

set -e

# Build the compiler
opam exec -- dune build

# Check if a source file was provided
if [ -z "$1" ]; then
    echo "Usage: ./run.sh <source.coco>"
    exit 1
fi

# Get the source file path without extension
SOURCE="$1"
BASENAME="${SOURCE%.coco}"

# Compile the CocoScript source
opam exec -- dune exec cocoscript "$SOURCE"

# Run the compiled binary
if [ -f "$BASENAME" ]; then
    "./$BASENAME"
else
    echo "Error: Compiled binary not found at $BASENAME"
    exit 1
fi
