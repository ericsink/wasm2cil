#!/bin/sh
clang --sysroot=/mnt/c/Users/eric/dev/wasi-sysroot/sysroot --target=wasm32-unknown-wasi -O3 miniray.c -ffast-math -Wl,--no-entry -Wl,--export=miniray -Wl,--allow-undefined  -o miniray.wasm
../../wabt/bin/wasm2wat miniray.wasm > miniray.wat

