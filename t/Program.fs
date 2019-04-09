
open System

open wasm.read_basic
open wasm.read
open wasm.write
open wasm.cecil
open Builders

[<EntryPoint>]
let main argv =
    let assy = System.Reflection.Assembly.GetAssembly(typeof<env>)

    let br = BinaryWasmStream(System.IO.File.ReadAllBytes(argv.[0]))
    let timer = System.Diagnostics.Stopwatch.StartNew()
    let m = read_module br
    timer.Stop()
    //printfn "%A milliseconds" timer.ElapsedMilliseconds

    //let m = build_simple_callindirect 42 7

    //printfn "%A" m

    let ba = 
        use ms = new System.IO.MemoryStream()
        let id = "hello"
        let ns = id
        let classname = "foo"
        let ver = new System.Version(1, 0, 0, 0)
        gen_assembly assy m id ns classname ver ms
        ms.ToArray()
    System.IO.File.WriteAllBytes("hello.dll", ba)

    // TODO write

    if argv.Length > 1 then
        let ba = 
            use ms = new System.IO.MemoryStream()
            use w = new System.IO.BinaryWriter(ms)
            write_module w m
            ms.ToArray()
        System.IO.File.WriteAllBytes(argv.[1], ba)

    0 // return an integer exit code
