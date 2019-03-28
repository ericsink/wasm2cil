// Learn more about F# at http://fsharp.org

open System
open System.IO

open wasm.buffer
open wasm.m

[<EntryPoint>]
let main argv =
    let br = BinaryWasmStream(File.ReadAllBytes(argv.[0]))
    let m = read_module br
    printfn "%A" m
    0 // return an integer exit code
