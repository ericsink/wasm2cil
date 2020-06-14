#!/bin/sh
set -e
cd ../../rust-raytracer
cargo wasi build --release
cd ../wasm2cil/tool
cp ../../rust-raytracer/target/wasm32-wasi/release/raytracer.wasm .
ls -l ./raytracer.wasm

