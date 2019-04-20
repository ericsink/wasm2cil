#!/bin/sh
clang --sysroot=/mnt/c/Users/eric/dev/wasi-sysroot/sysroot --target=wasm32-unknown-wasi -O2 $1 -Wl,--export-all -Wl,--allow-undefined  -o $1.wasm
../../wabt/bin/wasm2wat $1.wasm > $1.wat

