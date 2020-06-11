#!/bin/sh
set -e
ls -l $1
echo "transpiling"
wasmname=$1
basename="${wasmname%.*}"
dllname="${basename}.dll"
ilname="${basename}.il"
dotnet run -- compile $wasmname $dllname
ls -l $dllname
dotnet ildasm $dllname > $ilname
ls -l $ilname

