# wasm2cil

This is an implementation of WebAssembly and WASI for 
.NET.

Specifically, it can take a WebAssembly module and
"compile" it into a .NET assembly on disk.

There is also a C# implementation of WASI here.

This is designed to be used with Clang 8 and
wasi-sysroot.

All of this is very much a work in progress.

But the current status is that some basic things
do work.  Examples in this repo include a small
ray tracer and a basic port of SQLite, both of
which are working.

Still, don't try using this for anything serious yet.

See my blog post for additional details.

