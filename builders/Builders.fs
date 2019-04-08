module Builders

open System

open wasm.def_basic
open wasm.def_instr
open wasm.def
open wasm.read
open wasm.write
open wasm.cecil
open wasm.builder

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

