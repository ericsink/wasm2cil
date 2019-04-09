
open System

open wasm.read_basic
open wasm.read
open wasm.write
open wasm.cecil
open Builders

[<EntryPoint>]
let main argv =
    printfn "Args: (filename|build)  [assembly|None] [wasm|filename]"
    let assy = System.Reflection.Assembly.GetAssembly(typeof<env>)

    let m =
        match argv.[0] with
        | "build" ->
            build_module_invalid_block_type
        | filename ->
            let br = BinaryWasmStream(System.IO.File.ReadAllBytes(filename))
            let timer = System.Diagnostics.Stopwatch.StartNew()
            let m = read_module br
            timer.Stop()
            //printfn "%A milliseconds" timer.ElapsedMilliseconds
            m

    //printfn "%A" m

    if (argv.Length > 1) && (argv.[1] <> "None") then
        let ba = 
            use ms = new System.IO.MemoryStream()
            let id = "hello"
            let ns = id
            let classname = "foo"
            let ver = new System.Version(1, 0, 0, 0)
            gen_assembly assy m id ns classname ver ms
            ms.ToArray()
        let name = argv.[1]
        System.IO.File.WriteAllBytes(name, ba)

    if argv.Length > 2 then
        let ba = 
            use ms = new System.IO.MemoryStream()
            use w = new System.IO.BinaryWriter(ms)
            write_module w m
            ms.ToArray()
        let name = argv.[2]
        System.IO.File.WriteAllBytes(argv.[2], ba)

    0 // return an integer exit code

