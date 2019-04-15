#!/bin/sh
clang -DSQLITE_OS_OTHER ../sqlite3.c ../wrap.c run.c -lm

