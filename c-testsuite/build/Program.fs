
open System

open wasm.read_basic
open wasm.read
open wasm.write
open wasm.cecil

[<EntryPoint>]
let main argv =
    let assy = System.Reflection.Assembly.GetAssembly(typeof<wasi_unstable>)

    let m =
        let filename = argv.[0]
        printfn "Reading %s" filename
        let br = BinaryWasmStream(System.IO.File.ReadAllBytes(filename))
        let timer = System.Diagnostics.Stopwatch.StartNew()
        let m = read_module br
        timer.Stop()
        //printfn "%A milliseconds" timer.ElapsedMilliseconds
        m

    //printfn "%A" m

    // TODO arguably this should always just write to ..\run_test\test.dll
    let destname = argv.[1]
    printfn "Generating assembly %s" destname
    let ba = 
        use ms = new System.IO.MemoryStream()
        let id = "test"
        let ns = id
        let classname = "foo"
        let ver = new System.Version(1, 0, 0, 0)
        gen_assembly (Wasi assy) m id ns classname ver ms
        ms.ToArray()
    System.IO.File.WriteAllBytes(destname, ba)

    0 // return an integer exit code

