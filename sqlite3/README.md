
This directory is where I am working toward getting sqlite
to build and run.

sqlite3.c is an unmodified copy of the SQLite amalgamation 
version 3.26.0

vfs.c is the SQLite "demo" VFS, slightly modified to remove
the call to getcwd().  Note that this VFS is lacking a lot
of functionality, so it's just a placeholder I am using for
development.

shell.c is the SQLite shell, with several modifications needed
for the Wasi platform.

to_wasm.sh will use clang 8 to compile the code to create
sqlite3.wasm

in ./build is a little program that converts sqlite3.wasm into
sqlite3.dll, a .NET assembly.

in ./go is a little program to run sqlite3.dll

The result is the SQLite shell application, running in entirely
managed code.  For basic cases, it kinda works.  For example:

    dotnet run test.sqlite

    type SQL statements to create a table and insert some rows

    .quit

    then, using the regular SQLite shell, open test.sqlite
    and see that the managed code has written a database that
    seems valid.

