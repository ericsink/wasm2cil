#!/bin/sh
clang --target=wasm32 -O3 foo.c -ffast-math -nostdlib -Wl,--no-entry -Wl,--export=miniray -Wl,--allow-undefined  -o miniray.wasm
../../wabt/bin/wasm2wat miniray.wasm > miniray.wat

