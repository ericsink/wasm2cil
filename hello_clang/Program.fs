
open System

open wasm.read_basic
open wasm.read
open wasm.write
open wasm.cecil

[<EntryPoint>]
let main argv =
    let assy = System.Reflection.Assembly.GetAssembly(typeof<env>)

    // now load the wasm file,, arg 1

    let br = BinaryWasmStream(System.IO.File.ReadAllBytes(argv.[0]))
    let timer = System.Diagnostics.Stopwatch.StartNew()
    let m = read_module br
    timer.Stop()
    //printfn "%A milliseconds" timer.ElapsedMilliseconds

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

    0 // return an integer exit code
