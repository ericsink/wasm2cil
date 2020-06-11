#!/bin/sh
set -e
echo "running"
time dotnet run -- rundll raytracer.dll > wasm2cil.ppm
ls -l wasm2cil.ppm

