#!/bin/sh
dotnet run -- compile ../../rust-raytracer/target/wasm32-wasi/release/rust-raytracer.rustc.wasm raytracer.dll
time dotnet run -- rundll raytracer.dll

