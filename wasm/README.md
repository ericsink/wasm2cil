
This is the main compiler that converts a Wasm module
to a .NET assembly.

There is code here to read the binary encoding of a Wasm
module and convert it to an internal representation.

There is also code to take that representation and write
it back out to a binary Wasm module, but I use that only
for testing.

The real purpose here is to write the Wasm module back
out as a .NET assembly, an actual .DLL file on the filesystem.
This compiler uses Mono.Cecil as the backend for that.


