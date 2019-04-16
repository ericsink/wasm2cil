#!/bin/sh
clang --sysroot=/mnt/c/Users/eric/dev/wasi-sysroot/sysroot --target=wasm32-unknown-wasi -O2 vfs.c os.c wrap.c sqlite3.c -DSQLITE_OS_OTHER -DSQLITE_OMIT_FLOATING_POINT -Wl,--no-entry -Wl,--export-all -Wl,--allow-undefined  -o sqlite3.wasm
../../wabt/bin/wasm2wat sqlite3.wasm > sqlite3.wat

