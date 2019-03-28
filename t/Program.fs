// Learn more about F# at http://fsharp.org

open System

open wasm.buffer
open wasm.m

[<EntryPoint>]
let main argv =
    let br = BinaryWasmStream(System.IO.File.ReadAllBytes(argv.[0]))
    let timer = System.Diagnostics.Stopwatch.StartNew()
    let m = read_module br
    timer.Stop()
    printfn "%A milliseconds" timer.ElapsedMilliseconds
    printfn "%A" m
    0 // return an integer exit code
