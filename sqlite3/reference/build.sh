#!/bin/sh
clang -O2 -DSQLITE_OS_OTHER ../vfs.c ../os.c ../wrap.c ../sqlite3.c -lm

