#!/bin/sh
clang --target=wasm32 -O3 sqlite3.c -DSQLITE_OS_OTHER -DSQLITE_OMIT_FLOATING_POINT -nostdlib -Wl,--no-entry -Wl,--export-all -Wl,--allow-undefined  -o sqlite3.wasm
../../wabt/bin/wasm2wat sqlite3.wasm > sqlite3.wat

