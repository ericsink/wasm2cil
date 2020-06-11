#!/bin/sh
set -e
pnmtopng wasmtime.ppm > wasmtime.png
ls -l wasmtime.png
pnmtopng wasmer.ppm > wasmer.png
ls -l wasmer.png
