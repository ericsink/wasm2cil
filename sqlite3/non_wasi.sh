#!/bin/sh
clang --target=wasm32 -O2 sqlite3.c -DNDEBUG -DSQLITE_OS_UNIX -DSQLITE_THREADSAFE=0 -nostdlib -Wl,--export-all -Wl,--no-entry -Wl,--allow-undefined -Wl,--no-threads -o sqlite3.wasm
../../wabt/bin/wasm2wat sqlite3.wasm > sqlite3.wat

