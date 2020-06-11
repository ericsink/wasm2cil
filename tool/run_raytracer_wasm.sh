#!/bin/sh
set -e
echo "running with wasmtime..."
wasmtime raytracer.wasm > wasmtime.ppm
ls -l wasmtime.ppm
echo "running with wasmer..."
wasmer raytracer.wasm > wasmer.ppm
ls -l wasmer.ppm

