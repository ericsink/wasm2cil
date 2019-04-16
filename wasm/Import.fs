
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
        if typ = null then
            // TODO throw a more specific exception
            failwith (sprintf "import module not found: %s" s.m)
        let parms = 
            s.typ.parms
            |> Array.map valtype_to_basic_type
        let method =
            let method = typ.GetMethod(s.name, parms)
            if method <> null then
                method
            else
                // TODO throw a more specific exception
                failwith (sprintf "import method not found: %A %s.%s(%A)" s.typ.result s.m s.name s.typ.parms)
        // TODO fail if the return type is wrong
        let mref = md.ImportReference(method)
        mref

    let import_memory (md : ModuleDefinition) (s : ImportedMemory) (a : System.Reflection.Assembly) =
        let typ = a.GetType(s.m)
        let f = typ.GetField(s.name)
        // TODO complain if the field is not intptr ?
        if f = null then
            failwith (sprintf "import memory not found: %s.%s" s.m s.name)
        let mref = md.ImportReference(f)
        mref

    let import_global (md : ModuleDefinition) (s : ImportedGlobal) (a : System.Reflection.Assembly) =
        let typ = a.GetType(s.m)
        let f = typ.GetField(s.name)
        // TODO complain if the field type is wrong ?
        if f = null then
            failwith (sprintf "import global not found: %s.%s" s.m s.name)
        let mref = md.ImportReference(f)
        mref

