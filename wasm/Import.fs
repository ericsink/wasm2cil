
module wasm.import

    open System
    open System.Reflection

    open Mono.Cecil

    open wasm.def_basic
    open wasm.module_index

    type ReferencedAssembly = {
        assembly : System.Reflection.Assembly
        typename : string
        }

    let valtype_to_basic_type t =
        match t with
        | I32 -> typeof<int32>
        | I64 -> typeof<int64>
        | F32 -> typeof<float32>
        | F64 -> typeof<double>

    let import_function_simple (md : ModuleDefinition) a (func_name : string) =
        let typ = a.assembly.GetType(a.typename)
        if typ = null then
            // TODO throw a more specific exception
            failwith (sprintf "import module not found: %s" a.typename)
        let method = typ.GetMethod(func_name)
        if method = null then
            None
        else
            let mref = md.ImportReference(method)
            Some mref

    let import_function (md : ModuleDefinition) a (result_type : System.Type) (func_name : string) (parms : System.Type[]) =
        let typ = a.assembly.GetType(a.typename)
        if typ = null then
            // TODO throw a more specific exception
            failwith (sprintf "import module not found: %s" a.typename)
        let method = typ.GetMethod(func_name, parms)
        if method = null then
            None
        else
            // TODO fail if the return type is wrong
            let mref = md.ImportReference(method)
            Some mref

    let import_function_old (md : ModuleDefinition) (s : ImportedFunc) (a : System.Reflection.Assembly) =
        let typ = a.GetType(s.m)
        let typ = a.GetType("wasi_unstable") // TODO temporary hack
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

    let import_field (md : ModuleDefinition) a (field_name : string) =
        let typ = a.assembly.GetType(a.typename)
        if typ = null then
            // TODO throw a more specific exception
            failwith (sprintf "import module not found: %s" a.typename)
        let f = typ.GetField(field_name)
        if f = null then
            None
        else
            let mref = md.ImportReference(f)
            Some mref

    let import_field_old (md : ModuleDefinition) (type_name : string) (field_name : string) (a : System.Reflection.Assembly) =
        let typ = a.GetType(type_name)
        let f = typ.GetField(field_name)
        // TODO complain if the field is not intptr ?
        if f = null then
            failwith (sprintf "import field not found: %s.%s" type_name field_name)
        let mref = md.ImportReference(f)
        mref

    let import_memory (md : ModuleDefinition) (s : ImportedMemory) (a : System.Reflection.Assembly) =
        // TODO use import_field
        let typ = a.GetType(s.m)
        let f = typ.GetField(s.name)
        // TODO complain if the field is not intptr ?
        if f = null then
            failwith (sprintf "import memory not found: %s.%s" s.m s.name)
        let mref = md.ImportReference(f)
        mref

    let import_global (md : ModuleDefinition) (s : ImportedGlobal) (a : System.Reflection.Assembly) =
        // TODO use import_field
        let typ = a.GetType(s.m)
        let f = typ.GetField(s.name)
        // TODO complain if the field type is wrong ?
        if f = null then
            failwith (sprintf "import global not found: %s.%s" s.m s.name)
        let mref = md.ImportReference(f)
        mref

    let refs_import_function_simple (md : ModuleDefinition) (references : ReferencedAssembly[]) (func_name : string) =
        Array.tryPick 
            (fun a ->
                import_function_simple md a func_name
                ) references

    let refs_import_function (md : ModuleDefinition) (references : ReferencedAssembly[]) (result_type : System.Type) (func_name : string) (parms : System.Type[]) =
        Array.tryPick 
            (fun a ->
                import_function md a result_type func_name parms
                ) references

    let refs_import_field (md : ModuleDefinition) (references : ReferencedAssembly[]) (field_name : string) =
        Array.tryPick 
            (fun a ->
                import_field md a field_name
                ) references


