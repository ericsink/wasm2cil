
These scripts make it possible to run the following test suite:

https://github.com/c-testsuite/c-testsuite

Basically, I compile each of the 220 test cases using clang 8:

clang --sysroot=/mnt/c/Users/eric/dev/wasi-sysroot/sysroot --target=wasm32-unknown-wasi -O2 "$t" -Wl,--allow-undefined  -o "$t.wasm"

And then I use my wasm2cil compiler in conjunction with my C#
implementation of Wasi to run all the tests and compare the output
of each one to the expected output given in c-testsuite.

At this time, wasm2cil passes 219 out of the 220 test cases.
The only failing test appears to be something about my build
of wasi-sysroot not being compiled with support for formatting
of long double.

