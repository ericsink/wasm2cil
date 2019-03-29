// Learn more about F# at http://fsharp.org

open System

open wasm.read_basic
open wasm.read
open wasm.write
open wasm.wat

[<EntryPoint>]
let main argv =
    let br = BinaryWasmStream(System.IO.File.ReadAllBytes(argv.[0]))
    let timer = System.Diagnostics.Stopwatch.StartNew()
    let m = read_module br
    timer.Stop()
    printfn "%A milliseconds" timer.ElapsedMilliseconds

    //printfn "%A" m
    wat_module m

    use ms = new System.IO.MemoryStream()
    use w = new System.IO.BinaryWriter(ms)
    write_module w m
    let ba = ms.ToArray()
    System.IO.File.WriteAllBytes(argv.[1], ba)

    0 // return an integer exit code
