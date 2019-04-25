#!/bin/sh
clang -O2 -DSQLITE_OS_OTHER ../vfs.c ../os.c ../shell.c ../sqlite3.c -lm

