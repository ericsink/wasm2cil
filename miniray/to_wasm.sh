#!/bin/sh
clang --target=wasm32 -O3 miniray.c -ffast-math -nostdlib -Wl,--no-entry -Wl,--export=miniray -Wl,--allow-undefined  -o miniray.wasm

