#!/bin/sh
clang --sysroot=/mnt/c/Users/eric/dev/wasi-sysroot/sysroot --target=wasm32-unknown-wasi -O2 $1 -Wl,--allow-undefined  -o $1.wasm

