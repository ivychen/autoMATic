#!/bin/bash

LLI="lli-5.0"
LLC="llc-5.0"
CC="gcc"
MICROC="./automatic.native"

ulimit -t 30

TARGET="$1"

eval "$MICROC $TARGET > out.ll" # compile ic to LLVM IR
eval "$LLC --relocation-model=pic -o out.s out.ll"
eval "$CC -o out out.s"
eval "./out; rm out.ll out.s out"
