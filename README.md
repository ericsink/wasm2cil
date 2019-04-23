# wasm2cil

This is a .NET compiler for WebAssembly modules, including
support for WASI.

Specifically, it can take a WebAssembly module and
translate it into a .NET assembly on disk.

The resulting DLL is not interpreted -- it contains
the same functions as the WebAssembly module, translated
from Wasm instructions to CIL instructions.

This is designed to be used with Clang 8 and
wasi-sysroot.  In that sense, this provides a way
to compile C/C++ for .NET, using Wasm as a halfway
point.  Like an alternative to pinvoke.

(Rust should hopefully work too, but I haven't tried that yet.)

All of this is very much a work in progress.

But the current status is that some basic things
do work.  Working test apps in this repo include a 
small ray tracer and a basic port of SQLite.  The
latter can read and write SQLite database files that
seem to be fully interoperable with "regular" SQLite
builds.

Still, I suggest you not try using this for anything serious yet.

The subdirectories of this repo have README files with
a bit more info.

See [my blog entry](https://ericsink.com/entries/wasm_wasi_dotnet.html) for more information.
