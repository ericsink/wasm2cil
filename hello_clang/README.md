
TODO but how do we get clang to mark things exported ?

clang-8 --target=wasm32 -O3 -c hello.c -o hello.wasm

dotnet run ./hello.wasm

csc hola.cs -r:hello.dll

make sure hola.runtimeconfig.json exists

copy ../env/bin/Debug/netstandard2.0/env.dll .

dotnet hola.exe


