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

type AssemblyStuff = {
    a : System.Reflection.Assembly
    ns : string
    classname : string
    }

let prep_assembly m =
    let id = newid ()
    let ns = "test_namespace"
    let classname = "foo"
    let ver = new System.Version(1, 0, 0, 0)
    let ba = 
        use ms = new System.IO.MemoryStream()
        gen_assembly m id ns classname ver ms
        ms.ToArray()
    let a = System.Reflection.Assembly.Load(ba)
    Assert.NotNull(a)
    { a = a; classname = classname; ns = ns; }

let get_method a name =
    let fullname = sprintf "%s.%s" a.ns a.classname
    let t = a.a.GetType(fullname)
    let mi = t.GetMethod(name)
    Assert.NotNull(mi)
    mi

let check_0 (mi : System.Reflection.MethodInfo) (f : Unit -> 'b) =
    let r = mi.Invoke(null, null)
    let x = unbox<'b> r
    let should_be = f ()
    let eq = should_be = x
    Assert.True(eq)

let check_1 (mi : System.Reflection.MethodInfo) (f : 'a -> 'b) (n : 'a) =
    let args = [| box n |]
    let r = mi.Invoke(null, args)
    let x = unbox<'b> r
    let should_be = f n
    let eq = should_be = x
    Assert.True(eq)

[<Fact>]
let ``empty module`` () =
    let m = {
        version = 1u
        sections = Array.empty
        }

    let a = prep_assembly m
    Assert.NotNull(a.a)

[<Fact>]
let ``empty method`` () =
    let fb = FunctionBuilder()
    let name = "empty"
    fb.Name <- Some name
    fb.ReturnType <- None
    fb.Add (End)

    let b = ModuleBuilder()
    b.AddFunction(fb)

    let m = b.CreateModule()

    let a = prep_assembly m
    let mi = get_method a name
    mi.Invoke(null, null) |> ignore
    Assert.True(true)

[<Fact>]
let ``int constant`` () =
    let fb = FunctionBuilder()
    let num = 42
    let name = "constant"
    fb.Name <- Some name
    fb.ReturnType <- Some I32
    fb.Add (I32Const num)
    fb.Add (End)

    let b = ModuleBuilder()
    b.AddFunction(fb)

    let m = b.CreateModule()

    let a = prep_assembly m
    let mi = get_method a name

    let impl n =
        num

    check_0 mi impl

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
    fb.Add (End)

    let b = ModuleBuilder()
    b.AddFunction(fb)

    let m = b.CreateModule()

    let a = prep_assembly m
    let mi = get_method a name

    let impl n =
        n + addnum

    let check =
        check_1 mi impl

    check 13
    check 22

[<Fact>]
let ``simple loop optimized out`` () =
    (*

int foo(int x)
{
    int r = 0;
    for (int i=0; i<x; i++)
    {
        r += i;
    }
    return r;
}

    *)

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

    let a = prep_assembly m
    let mi = get_method a name

    let impl x =
        let mutable r = 0
        for i = 0 to (x - 1) do
            r <- r + i
        r

    let check =
        check_1 mi impl

    check 0
    check 1
    check 13
    check -5
    check 22

