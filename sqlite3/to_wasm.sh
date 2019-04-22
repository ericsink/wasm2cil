#!/bin/sh
clang --sysroot=/mnt/c/Users/eric/dev/wasi-sysroot/sysroot --target=wasm32-unknown-wasi -O2 vfs.c os.c shell.c sqlite3.c -DSQLITE_OS_OTHER -Wl,--export-all -Wl,--allow-undefined -Wl,--no-threads -o sqlite3.wasm
../../wabt/bin/wasm2wat sqlite3.wasm > sqlite3.wat

