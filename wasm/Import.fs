
module wasm.import

    open System
    open System.Reflection

    open Mono.Cecil

    open wasm.def_basic
    open wasm.module_index

    let valtype_to_basic_type t =
        match t with
        | I32 -> typeof<int32>
        | I64 -> typeof<int64>
        | F32 -> typeof<float32>
        | F64 -> typeof<double>

    let import_function (md : ModuleDefinition) (s : ImportedFunc) (a : System.Reflection.Assembly) =
        let typ = a.GetType(s.m)
        let parms = 
            s.typ.parms
            |> Array.map valtype_to_basic_type
        let method = typ.GetMethod(s.name, parms)
        let mref = md.ImportReference(method)
        mref

    let import_memory (md : ModuleDefinition) (s : ImportedMemory) (a : System.Reflection.Assembly) =
        let typ = a.GetType(s.m)
        let f = typ.GetField(s.name)
        // TODO complain if the field is not intptr ?
        let mref = md.ImportReference(f)
        mref

    let import_global (md : ModuleDefinition) (s : ImportedGlobal) (a : System.Reflection.Assembly) =
        let typ = a.GetType(s.m)
        let f = typ.GetField(s.name)
        // TODO complain if the field type is wrong ?
        let mref = md.ImportReference(f)
        mref

