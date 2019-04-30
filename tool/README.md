
This is a command-line app for compiling 
Wasm modules into .NET assemblies.

If you `dotnet run` in this directory, the app will
show you usage information.

The "compile" subcommand is primary use case.  For example, to compile foo.wasm into a .NET assembly called foo.dll:

    dotnet run -- compile foo.wasm foo.dll

There is also a "run" subcommand, which is a convenient way to compile-and-run a given Wasm module.  The Wasm module is compiled to an assembly on each use, and the compiled assembly is not saved:

    dotnet run -- run foo.wasm

