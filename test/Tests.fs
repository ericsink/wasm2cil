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

[<Fact>]
let ``simple loop optimized out`` () =
    let fb = FunctionBuilder()
    let name = "foo"
    fb.Name <- Some name
    fb.ReturnType <- Some I32
    fb.AddParam I32

    fb.Add (Block (Some I32))
    fb.Add (LocalGet (LocalIdx 0u))
    fb.Add (I32Const 1)
    fb.Add (I32LtS)
    fb.Add (BrIf (LabelIdx 0u))
    fb.Add (LocalGet (LocalIdx 0u))
    fb.Add (I32Const -1)
    fb.Add (I32Add)
    fb.Add (I64ExtendI32U)
    fb.Add (LocalGet (LocalIdx 0u))
    fb.Add (I32Const -2)
    fb.Add (I32Add)
    fb.Add (I64ExtendI32U)
    fb.Add (I64Mul)
    fb.Add (I64Const 1L)
    fb.Add (I64ShrU)
    fb.Add (I32WrapI64)
    fb.Add (LocalGet (LocalIdx 0u))
    fb.Add (I32Add)
    fb.Add (I32Const -1)
    fb.Add (I32Add)
    fb.Add (Return)
    fb.Add (End)
    fb.Add (I32Const 0)
    fb.Add (End)

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

    let check x =
        let should =
            let mutable r = 0
            for i = 0 to (x - 1) do
                r <- r + i
            r
        let args = [| box x |]
        let r = mi.Invoke(null, args)
        let x = unbox<int> r
        Assert.Equal(should, x)

    check 0
    check 1
    check 13
    check -5
    check 22

    Assert.True(true)

