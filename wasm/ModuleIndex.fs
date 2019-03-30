
module wasm.module_index

    open wasm.def
    open wasm.def_basic

    type ModuleIndex = {
        Type : TypeSection option
        Import : ImportSection option
        Function : FunctionSection option
        Table : TableSection option
        Memory : MemorySection option
        Global : GlobalSection option
        Export : ExportSection option
        Start : FuncIdx option
        Element : ElementSection option
        Code : CodeSection option
        Data : DataSection option
        // TODO list of custom sections?
        }

    let get_module_index m =
        {
            Type = Array.tryPick (fun x -> match x with | Type i -> Some i | _ -> None) m.sections
            Import = Array.tryPick (fun x -> match x with | Import i -> Some i | _ -> None) m.sections
            Function = Array.tryPick (fun x -> match x with | Function i -> Some i | _ -> None) m.sections
            Table = Array.tryPick (fun x -> match x with | Table i -> Some i | _ -> None) m.sections
            Memory = Array.tryPick (fun x -> match x with | Memory i -> Some i | _ -> None) m.sections
            Global = Array.tryPick (fun x -> match x with | Global i -> Some i | _ -> None) m.sections
            Export = Array.tryPick (fun x -> match x with | Export i -> Some i | _ -> None) m.sections
            Start = Array.tryPick (fun x -> match x with | Start i -> Some i | _ -> None) m.sections
            Element = Array.tryPick (fun x -> match x with | Element i -> Some i | _ -> None) m.sections
            Code = Array.tryPick (fun x -> match x with | Code i -> Some i | _ -> None) m.sections
            Data = Array.tryPick (fun x -> match x with | Data i -> Some i | _ -> None) m.sections
        }

    let get_function_name ndx i =
        let fmatch (x : ExportItem) =
            match x.desc with
            | ExportFunc n when n = i -> Some x.name
            | _ -> None

        match ndx.Export with
        | Some s -> Array.tryPick fmatch s.exports
        | None -> None

    let get_function_type ndx typeidx =
        let (TypeIdx i) = typeidx
        match ndx.Type with
        | Some s -> Some (s.types.[int i])
        | None -> None

