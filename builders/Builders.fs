module Builders

open System

open wasm.def_basic
open wasm.def_instr
open wasm.def
open wasm.read
open wasm.write
open wasm.cecil
open wasm.builder

let build_module (fb : FunctionBuilder) =
    let b = ModuleBuilder()
    b.AddFunction(fb)
    let m = b.CreateModule()
    m

let build_function_conv name t_from t_to op =
    let fb = FunctionBuilder()
    fb.Name <- Some name
    fb.ReturnType <- Some t_to
    fb.AddParam t_from
    fb.Add (LocalGet (LocalIdx 0u))
    fb.Add op
    fb.Add (End)
    fb

let build_function_f32_load name =
    let fb = FunctionBuilder()
    fb.Name <- Some name
    fb.ReturnType <- Some F32
    fb.AddParam I32
    fb.Add (LocalGet (LocalIdx 0u))
    fb.Add (F32Load { align=0u; offset=0u; })
    fb.Add (End)
    fb

let build_function_f32_store name =
    let fb = FunctionBuilder()
    fb.Name <- Some name
    fb.ReturnType <- None
    fb.AddParam I32
    fb.AddParam F32
    fb.Add (LocalGet (LocalIdx 0u))
    fb.Add (LocalGet (LocalIdx 1u))
    fb.Add (F32Store { align=0u; offset=0u; })
    fb.Add (End)
    fb

let build_function_i32_store name =
    let fb = FunctionBuilder()
    fb.Name <- Some name
    fb.ReturnType <- None
    fb.AddParam I32
    fb.AddParam I32
    fb.Add (LocalGet (LocalIdx 0u))
    fb.Add (LocalGet (LocalIdx 1u))
    fb.Add (I32Store { align=0u; offset=0u; })
    fb.Add (End)
    fb

let build_function_i64_store name =
    let fb = FunctionBuilder()
    fb.Name <- Some name
    fb.ReturnType <- None
    fb.AddParam I32
    fb.AddParam I64
    fb.Add (LocalGet (LocalIdx 0u))
    fb.Add (LocalGet (LocalIdx 1u))
    fb.Add (I64Store { align=0u; offset=0u; })
    fb.Add (End)
    fb

let build_function_f64_load name =
    let fb = FunctionBuilder()
    fb.Name <- Some name
    fb.ReturnType <- Some F64
    fb.AddParam I32
    fb.Add (LocalGet (LocalIdx 0u))
    fb.Add (F64Load { align=0u; offset=0u; })
    fb.Add (End)
    fb

let build_function_f64_store name =
    let fb = FunctionBuilder()
    fb.Name <- Some name
    fb.ReturnType <- None
    fb.AddParam I32
    fb.AddParam F64
    fb.Add (LocalGet (LocalIdx 0u))
    fb.Add (LocalGet (LocalIdx 1u))
    fb.Add (F64Store { align=0u; offset=0u; })
    fb.Add (End)
    fb

let build_function_simple_loop_optimized_out name =
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
    fb.Name <- Some name
    fb.ReturnType <- Some I32
    fb.AddParam I32

    fb.Add (Block None)
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
    fb

let build_module_simple_loop_optimized_out name =
    let fb = build_function_simple_loop_optimized_out name
    let b = ModuleBuilder()
    b.AddFunction(fb)
    let m = b.CreateModule()
    m

let build_function_block_stack_underflow =
    let fb = FunctionBuilder()
    let name = "block_stack_underflow"
    fb.Name <- Some name
    fb.ReturnType <- Some I32
    fb.Add (I32Const 4)
    fb.Add (Block None)
    fb.Add (Drop)
    fb.Add (End)
    fb.Add (End)
    fb

let build_module_block_stack_underflow =
    let fb = build_function_block_stack_underflow
    let b = ModuleBuilder()
    b.AddFunction(fb)
    let m = b.CreateModule()
    m

let build_function_too_many_func_results =
    let fb = FunctionBuilder()
    let name = "too_many_func_results"
    fb.Name <- Some name
    fb.ReturnType <- Some I32
    fb.Add (I32Const 4)
    fb.Add (I32Const 8)
    fb.Add (End)
    fb

let build_module_too_many_func_results =
    let fb = build_function_too_many_func_results
    let b = ModuleBuilder()
    b.AddFunction(fb)
    let m = b.CreateModule()
    m

let build_function_too_many_block_results =
    let fb = FunctionBuilder()
    let name = "too_many_block_results"
    fb.Name <- Some name
    fb.ReturnType <- Some I32
    fb.Add (Block (Some I32))
    fb.Add (I32Const 4)
    fb.Add (I32Const 8)
    fb.Add (End)
    fb.Add (End)
    fb

let build_module_too_many_block_results =
    let fb = build_function_too_many_block_results
    let b = ModuleBuilder()
    b.AddFunction(fb)
    let m = b.CreateModule()
    m

let build_function_invalid_block_type =
    let fb = FunctionBuilder()
    let name = "invalid_block_type"
    fb.Name <- Some name
    fb.ReturnType <- Some I32
    fb.Add (Block (Some I32))
    fb.Add (F64Const 3.14)
    fb.Add (End)
    fb.Add (End)
    fb

let build_module_invalid_block_type =
    let fb = build_function_invalid_block_type
    let b = ModuleBuilder()
    b.AddFunction(fb)
    let m = b.CreateModule()
    m

let build_function_add_value_from_block =
    let fb = FunctionBuilder()
    let name = "add_value_from_block"
    fb.Name <- Some name
    fb.ReturnType <- Some I32
    fb.AddParam I32
    fb.AddParam I32

    fb.Add (LocalGet (LocalIdx 0u))
    fb.Add (Block (Some I32))
    fb.Add (LocalGet (LocalIdx 1u))
    fb.Add (End)
    fb.Add (I32Add)
    fb.Add (End)
    fb

let build_module_add_value_from_block =
    let fb = build_function_add_value_from_block
    let b = ModuleBuilder()
    b.AddFunction(fb)
    let m = b.CreateModule()
    m

let build_simple_callindirect addnum mulnum =

    let fb_add = FunctionBuilder()
    let name = sprintf "add_%d" addnum
    fb_add.Name <- Some name
    fb_add.ReturnType <- Some I32
    fb_add.AddParam I32
    fb_add.Add (LocalGet (LocalIdx 0u))
    fb_add.Add (I32Const addnum)
    fb_add.Add I32Add
    fb_add.Add (End)

    let fb_mul = FunctionBuilder()
    let name = sprintf "mul_%d" mulnum
    fb_mul.Name <- Some name
    fb_mul.ReturnType <- Some I32
    fb_mul.AddParam I32
    fb_mul.Add (LocalGet (LocalIdx 0u))
    fb_mul.Add (I32Const mulnum)
    fb_mul.Add I32Mul
    fb_mul.Add (End)

    let fb_calc = FunctionBuilder()
    let name = "calc"
    fb_calc.Name <- Some name
    fb_calc.ReturnType <- Some I32
    fb_calc.AddParam I32 // val
    fb_calc.AddParam I32 // tableidx
    fb_calc.Add (LocalGet (LocalIdx 0u))
    fb_calc.Add (LocalGet (LocalIdx 1u))
    fb_calc.Add (CallIndirect { typeidx = TypeIdx 0u; other = 0uy; })
    fb_calc.Add (End)

    let b = ModuleBuilder()
    b.AddFunction(fb_add)
    b.AddFunction(fb_mul)
    b.AddFunction(fb_calc)

    b.AddElement(0, 0u)
    b.AddElement(1, 1u)

    let m = b.CreateModule()

    m

