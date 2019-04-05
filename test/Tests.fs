module Tests

open System
open Xunit
open wasm.def_basic
open wasm.def_instr
open wasm.def
open wasm.read
open wasm.write
open wasm.cecil
open wasm.builder

let newid () =
    Guid
        .NewGuid()
        .ToString()
        .Replace("{", "")
        .Replace("}", "")
        .Replace("-", "")

[<Fact>]
let ``empty module`` () =
    let m = {
        version = 1u
        sections = Array.empty
        }

    let id = newid ()
    let ver = new System.Version(1, 0, 0, 0)
    let ba = 
        use ms = new System.IO.MemoryStream()
        gen_assembly m id "my_namespace" "foo" ver ms
        ms.ToArray()
    System.Reflection.Assembly.Load(ba)

    Assert.True(true)

[<Fact>]
let ``simple add`` () =
    let fb = FunctionBuilder()
    let addnum = 42
    let name = sprintf "add_%d" addnum
    fb.Name <- Some name
    fb.ReturnType <- Some I32
    fb.AddParam I32
    fb.Add (LocalGet (LocalIdx 0u))
    fb.Add (I32Const addnum)
    fb.Add I32Add

    let b = ModuleBuilder()
    b.AddFunction(fb)

    let m = b.CreateModule()

    let ns = "my_namespace"
    let classname = "foo"
    let id = newid ()
    let ver = new System.Version(1, 0, 0, 0)
    let ba = 
        use ms = new System.IO.MemoryStream()
        gen_assembly m id ns classname ver ms
        ms.ToArray()
    let a = System.Reflection.Assembly.Load(ba)
    let fullname = sprintf "%s.%s" ns classname
    let t = a.GetType(fullname)
    let mi = t.GetMethod(name)
    Assert.NotNull(mi)

    let check n =
        let args = [| box n |]
        let r = mi.Invoke(null, args)
        let x = unbox<int> r
        Assert.Equal(n + addnum, x)

    check 13
    check 22

    Assert.True(true)

