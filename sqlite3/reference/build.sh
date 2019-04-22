#!/bin/sh
clang -DSQLITE_OS_OTHER ../vfs.c ../os.c ../shell.c ../sqlite3.c -lm

