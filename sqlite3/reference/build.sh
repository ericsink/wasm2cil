#!/bin/sh
clang -DSQLITE_OS_OTHER ../vfs.c ../os.c ../sqlite3.c ../wrap.c run.c -lm

