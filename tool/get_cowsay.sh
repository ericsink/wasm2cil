#!/bin/sh
set -e
wapm install cowsay
cp ./wapm_packages/_/cowsay@0.2.0/target/wasm32-wasi/release/cowsay.wasm .
ls -l ./cowsay.wasm

