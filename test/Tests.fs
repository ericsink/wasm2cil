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
open wasm.errors
open Builders

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
        // TODO mv this above to a static field?
        let assy = System.Reflection.Assembly.GetAssembly(typeof<env>)
        use ms = new System.IO.MemoryStream()
        gen_assembly assy m id ns classname ver ms
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
    let result = unbox<'b> r
    let should_be = f n
    Assert.Equal<'b>(should_be, result)

let check_2 (mi : System.Reflection.MethodInfo) (f : 'a -> 'a -> 'b) (x : 'a) (y : 'a) =
    let args = [| box x; box y |]
    let r = mi.Invoke(null, args)
    let result = unbox<'b> r
    let should_be = f x y
    Assert.Equal<'b>(should_be, result)

[<Fact>]
let empty_module () =
    let test_name = System.Reflection.MethodBase.GetCurrentMethod().Name
    let m = {
        version = 1u
        sections = Array.empty
        }

    let a = prep_assembly m
    Assert.NotNull(a.a)

[<Fact>]
let empty_method () =
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
let int_constant () =
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
let drop_empty () =
    let fb = FunctionBuilder()
    let name = "drop_empty"
    fb.Name <- Some name
    fb.ReturnType <- None
    fb.Add (Drop)
    fb.Add (End)

    let b = ModuleBuilder()
    b.AddFunction(fb)

    let m = b.CreateModule()

    Assert.Throws<OperandStackUnderflow>(fun () -> prep_assembly m |> ignore)

[<Fact>]
let memory_size () =
    let fb = FunctionBuilder()
    let name = "memory_size"
    fb.Name <- Some name
    fb.ReturnType <- Some I32
    fb.Add (MemorySize 0uy)
    fb.Add (End)

    let b = ModuleBuilder()
    b.AddFunction(fb)

    let m = b.CreateModule()

    let a = prep_assembly m
    let mi = get_method a name

    let impl n =
        1

    check_0 mi impl

[<Fact>]
let import_memory_store () =
    let fb = FunctionBuilder()
    let num = 42
    let name = "constant"
    fb.Name <- Some name
    fb.ReturnType <- Some I32
    fb.Add (I32Const num)
    fb.Add (End)

    let b = ModuleBuilder()
    b.AddFunction(fb)

    b.AddImport({ m = "env"; name = "__mem_only_imported_in_one_test"; desc = ImportMem { limits = Min 1u }; })

    let m = b.CreateModule()

    Assert.Equal(IntPtr.Zero, env.__mem_only_imported_in_one_test)

    let a = prep_assembly m
    let mi = get_method a name

    let impl n =
        num

    check_0 mi impl

    Assert.NotEqual(IntPtr.Zero, env.__mem_only_imported_in_one_test)

    let size = 64 * 1024
    // TODO Assert.Equal(size, env.__mem_only_imported_in_one_test.Length)
    let (za : byte[]) = Array.zeroCreate (64 * 1024)
    let (ta : byte[]) = Array.zeroCreate (64 * 1024)
    System.Runtime.InteropServices.Marshal.Copy(env.__mem_only_imported_in_one_test, ta, 0, size);
    Assert.Equal<byte[]>(za, ta)

[<Fact>]
let test_memory_load () =
    let name_k = "constant"
    let name_fetch = "fetch"
    let num_k = 42;

    let fb_k =
        let fb = FunctionBuilder()
        fb.Name <- Some name_k
        fb.ReturnType <- Some I32
        fb.Add (I32Const num_k)
        fb.Add (End)
        fb

    let fb_fetch =
        let fb = FunctionBuilder()
        fb.Name <- Some name_fetch
        fb.AddParam I32
        fb.ReturnType <- Some I32
        fb.Add (LocalGet (LocalIdx 0u))
        fb.Add (I32Load8U { align=0u; offset=0u; })
        fb.Add (End)
        fb

    let b = ModuleBuilder()
    b.AddFunction(fb_k)
    b.AddFunction(fb_fetch)

    // TODO note that all tests share the same instance of env.__my_mem

    b.AddImport({ m = "env"; name = "__my_mem"; desc = ImportMem { limits = Min 1u }; })

    let m = b.CreateModule()

    let a = prep_assembly m

    let mi_k = get_method a name_k

    let impl_k n =
        num_k

    check_0 mi_k impl_k

    let mi_fetch = get_method a name_fetch

    let store_byte (off : int32) (b : byte) =
        let ba = [| b |]
        System.Runtime.InteropServices.Marshal.Copy(ba, 0, env.__my_mem + (nativeint off), 1);

    store_byte 77 89uy

    let impl_fetch (off : int32) =
        let ba = [| 0uy |]
        System.Runtime.InteropServices.Marshal.Copy(env.__my_mem + (nativeint off), ba, 0, 1);
        ba.[0] |> int

    let check =
        check_1 mi_fetch impl_fetch

    check 77
    check 7632

[<Fact>]
let test_data () =
    let b = ModuleBuilder()
    let data = [| 1uy; 9uy; 8uy; 2uy; |]
    let off = 16309
    b.AddData(off, data)

    let m = b.CreateModule()

    let a = prep_assembly m

    let ba = env.GetResource(a.a, "data_0")

    Assert.Equal<byte[]>(data, ba)

[<Fact>]
let test_memory_load_data () =
    let name_fetch = "fetch"

    let fb_fetch =
        let fb = FunctionBuilder()
        fb.Name <- Some name_fetch
        fb.AddParam I32
        fb.ReturnType <- Some I32
        fb.Add (LocalGet (LocalIdx 0u))
        fb.Add (I32Load8U { align=0u; offset=0u; })
        fb.Add (End)
        fb

    let b = ModuleBuilder()
    b.AddFunction(fb_fetch)
    let data = [| 1uy; 9uy; 8uy; 2uy; |]
    let off = 16309
    b.AddData(off, data)

    let m = b.CreateModule()

    let a = prep_assembly m

    let mi_fetch = get_method a name_fetch

    let impl_fetch (x : int32) =
        data.[x - off] |> int

    let check =
        check_1 mi_fetch impl_fetch

    check (off + 0)

[<Fact>]
let simple_add () =
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
let add_with_incorrect_types () =
    let fb = FunctionBuilder()
    let name = "add"
    fb.Name <- Some name
    fb.ReturnType <- Some I32
    fb.AddParam I32
    fb.Add (LocalGet (LocalIdx 0u))
    fb.Add (F64Const 3.14)
    fb.Add I32Add
    fb.Add (End)

    let b = ModuleBuilder()
    b.AddFunction(fb)

    let m = b.CreateModule()

    Assert.Throws<WrongOperandType>(fun () -> prep_assembly m |> ignore)

[<Fact>]
let test_invalid_block_type () =
    let m = build_module_invalid_block_type
    Assert.Throws<WrongOperandType>(fun () -> prep_assembly m |> ignore)

[<Fact>]
let test_too_many_block_results () =
    let m = build_module_too_many_block_results
    // TODO review exception type here
    Assert.Throws<ExtraBlockResult>(fun () -> prep_assembly m |> ignore)

[<Fact>]
let test_too_many_func_results () =
    let m = build_module_too_many_func_results
    // TODO review exception type here
    Assert.Throws<ExtraBlockResult>(fun () -> prep_assembly m |> ignore)

[<Fact>]
let test_block_stack_underflow () =
    let m = build_module_block_stack_underflow
    // TODO review exception type here
    Assert.Throws<OperandStackUnderflow>(fun () -> prep_assembly m |> ignore)

(* TODO
[<Fact>]
let simple_add_with_block () =
    let fb = FunctionBuilder()
    let addnum = 42
    let name = sprintf "add_%d" addnum
    fb.Name <- Some name
    fb.ReturnType <- Some I32
    fb.AddParam I32
    fb.Add (LocalGet (LocalIdx 0u))
    fb.Add (Block None)
    fb.Add (I32Const 357)
    fb.Add (Br (LabelIdx 0u))
    fb.Add (F64Const 3.14)
    fb.Add (End)
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
*)

[<Fact>]
let simple_two_funcs () =

    let fb_add = FunctionBuilder()
    let addnum = 42
    let name = sprintf "add_%d" addnum
    fb_add.Name <- Some name
    fb_add.ReturnType <- Some I32
    fb_add.AddParam I32
    fb_add.Add (LocalGet (LocalIdx 0u))
    fb_add.Add (I32Const addnum)
    fb_add.Add I32Add
    fb_add.Add (End)

    let fb_mul = FunctionBuilder()
    let mulnum = 3
    let name = sprintf "mul_%d" mulnum
    fb_mul.Name <- Some name
    fb_mul.ReturnType <- Some I32
    fb_mul.AddParam I32
    fb_mul.Add (LocalGet (LocalIdx 0u))
    fb_mul.Add (I32Const mulnum)
    fb_mul.Add I32Mul
    fb_mul.Add (End)

    let fb_calc = FunctionBuilder()
    let subnum = 7
    let name = "calc"
    fb_calc.Name <- Some name
    fb_calc.ReturnType <- Some I32
    fb_calc.AddParam I32
    fb_calc.Add (LocalGet (LocalIdx 0u))
    fb_calc.Add (Call (FuncIdx 0u))
    fb_calc.Add (Call (FuncIdx 1u))
    fb_calc.Add (I32Const subnum)
    fb_calc.Add I32Sub
    fb_calc.Add (End)

    let b = ModuleBuilder()
    b.AddFunction(fb_add)
    b.AddFunction(fb_mul)
    b.AddFunction(fb_calc)

    let m = b.CreateModule()

    let a = prep_assembly m
    let mi = get_method a "calc"

    let impl n =
        ((n + addnum) * mulnum) - subnum

    let check =
        check_1 mi impl

    check -1
    check 0
    check 1
    check 13
    check 22

[<Fact>]
let simple_callindirect () =

    let addnum = 42
    let mulnum = 7
    let m = build_simple_callindirect addnum mulnum

    let a = prep_assembly m
    let mi = get_method a "calc"

    let impl w n =
        if n = 0 then
            w + addnum
        else if n = 1 then
            w * mulnum
        else
            failwith (sprintf "illegal: n=%d" n)

    let check =
        check_2 mi impl

    check 5 0
    check 5 1

[<Fact>]
let add_value_from_block () =

    let m = build_module_add_value_from_block
    let a = prep_assembly m
    let mi = get_method a "add_value_from_block"

    let impl x y = x + y

    let check =
        check_2 mi impl

    check 13 27
    check 2 2

let make_simple_compare_func name t op =
    let fb = FunctionBuilder()
    fb.Name <- Some name
    fb.ReturnType <- Some I32
    fb.AddParam t
    fb.AddParam t
    fb.Add (LocalGet (LocalIdx 0u))
    fb.Add (LocalGet (LocalIdx 1u))
    fb.Add op
    fb.Add (End)

    let b = ModuleBuilder()
    b.AddFunction(fb)

    let m = b.CreateModule()
    m

let make_simple_compare_check t wasm_op fs_op =
    let name = "compare"
    let m = make_simple_compare_func name t wasm_op
    let a = prep_assembly m
    let mi = get_method a name

    let impl a b =
        if fs_op a b then 1 else 0

    let check =
        check_2 mi impl

    check

[<Fact>]
let i32_gt () =
    let check = make_simple_compare_check I32 I32GtS (>)
    for x = -4 to 4 do
        for y = -4 to 4 do
            check x y

[<Fact>]
let i32_lt () =
    let check = make_simple_compare_check I32 I32LtS (<)
    for x = -4 to 4 do
        for y = -4 to 4 do
            check x y

[<Fact>]
let i32_ge () =
    let check = make_simple_compare_check I32 I32GeS (>=)
    for x = -4 to 4 do
        for y = -4 to 4 do
            check x y

[<Fact>]
let i32_le () =
    let check = make_simple_compare_check I32 I32LeS (<=)
    for x = -4 to 4 do
        for y = -4 to 4 do
            check x y

[<Fact>]
let simple_loop_optimized_out () =
    let name = "foo"
    let fb = build_function_simple_loop_optimized_out name
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

(*
let invoke_2 (mi : System.Reflection.MethodInfo) x y =
    let args = [| box x; box y |]
    let r = mi.Invoke(null, args)
    let result = unbox<'b> r
    result

[<Fact>]
let i32rotl () =
    let fb = FunctionBuilder()
    let name = "i32rotl"
    fb.Name <- Some name
    fb.ReturnType <- Some I32
    fb.AddParam I32
    fb.AddParam I32
    fb.Add (LocalGet (LocalIdx 0u))
    fb.Add (LocalGet (LocalIdx 1u))
    fb.Add (I32Rotl)
    fb.Add (End)

    let b = ModuleBuilder()
    b.AddFunction(fb)

    let m = b.CreateModule()
    let a = prep_assembly m
    let mi = get_method a name

    let impl v c =
        (v <<< c) ||| (v >>> (32 - c))

    let q = invoke_2 mi 7 32
    Assert.Equal(7, q)

    let check =
        check_2 mi impl

    check 17 4
    check 5 1
*)

