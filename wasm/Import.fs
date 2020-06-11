
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
        let typ = a.GetType("wasi_unstable") // TODO temporary hack
        if typ = null then
            None
        else
            let parms = 
                s.typ.parms
                |> Array.map valtype_to_basic_type
            let method = typ.GetMethod(s.name, parms)
            if method <> null then
                // TODO fail if the return type is wrong
                let mref = md.ImportReference(method)
                Some mref
            else
                // TODO throw a more specific exception
                failwith (sprintf "import method not found: %A %s.%s(%A)" s.typ.result s.m s.name s.typ.parms)

    let import_field (md : ModuleDefinition) (type_name : string) (field_name : string) (a : System.Reflection.Assembly) =
        let typ = a.GetType(type_name)
        if typ = null then
            None
        else
            let f = typ.GetField(field_name)
            // TODO complain if the field is not intptr ?
            if f = null then
                None
            else
                let mref = md.ImportReference(f)
                Some mref

    let refs_import_function (md : ModuleDefinition) (references : System.Reflection.Assembly[]) (s : ImportedFunc) =
        Array.tryPick 
            (fun a ->
                import_function md s a
                ) references

    let refs_import_field (md : ModuleDefinition) (references : System.Reflection.Assembly[]) (mod_name : string) (field_name : string) =
        Array.tryPick 
            (fun a ->
                import_field md mod_name field_name a
                ) references


