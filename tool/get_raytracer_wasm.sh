#!/bin/sh
set -e
cd ../../rust-raytracer/rayapp
cargo wasi build --release
cd ../../wasm2cil/tool
cp ../../rust-raytracer/rayapp/target/wasm32-wasi/release/rayapp.wasm ./raytracer.wasm
ls -l ./raytracer.wasm

