
clang --target=wasm32 -O3 sqlite3.c -DSQLITE_OS_OTHER -DSQLITE_OMIT_FLOATING_POINT -nostdlib -Wl,--no-entry -Wl,--export-all -Wl,--allow-undefined  -o sqlite3.wasm

need names to be clang and wasm-ld, clang-8 and wasm-ld-8
had to explicitly add /usr/lib/llvm-8/bin to the path

clang --target=wasm32 -O3 sqlite3.c -DSQLITE_OS_OTHER -nostdlib -Wl,--no-entry -Wl,--export-dynamic -Wl,--allow-undefined -o sql.wasm

need to run wasm-ld, the linker

--export-dynamic doesn't seem to preserve things.

maybe we need an explicit export list

--export=hello

clang --target=wasm32 -O3 hello.c -nostdlib -Wl,--no-entry -Wl,--export-all -Wl,--allow-undefined -o hello.wasm

clang-8 --target=wasm32 -O3 -c hello.c -o hello.wasm

dotnet run ./hello.wasm

csc hola.cs -r:hello.dll

make sure hola.runtimeconfig.json exists

copy ../env/bin/Debug/netstandard2.0/env.dll .

dotnet hola.exe


