
module wasm.module_index

    open wasm.def
    open wasm.def_basic

    type ImportedFunc = {
        f_m : string
        f_name : string
        f_typ : TypeIdx
        }

    type InternalFunc = {
        if_name : string option;
        if_typ : TypeIdx;
        }

    type FuncLookupItem =
        | F_Imported of ImportedFunc
        | F_Internal of InternalFunc

    type ImportedGlobal = {
        g_m : string
        g_name : string
        g_typ : GlobalType
        }

    type InternalGlobal = {
        ig_name : string option;
        ig_typ : GlobalType;
        }

    type GlobalLookupItem =
        | G_Imported of ImportedGlobal
        | G_Internal of InternalGlobal

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

        FuncLookup : FuncLookupItem[]
        GlobalLookup : GlobalLookupItem[]
        }

    let count_function_imports ndx =
        match ndx.Import with
        | Some si ->
            let a = Array.choose (fun i -> match i.desc with | ImportFunc typeidx -> Some 1 | _ -> None) si.imports
            a.Length
        | None -> 0

    let count_global_imports ndx =
        match ndx.Import with
        | Some si ->
            let a = Array.choose (fun i -> match i.desc with | ImportGlobal gt -> Some 1 | _ -> None) si.imports
            a.Length
        | None -> 0

    let get_module_index m =
        let s_import = Array.tryPick (fun x -> match x with | Import i -> Some i | _ -> None) m.sections
        let s_function = Array.tryPick (fun x -> match x with | Function i -> Some i | _ -> None) m.sections
        let s_export = Array.tryPick (fun x -> match x with | Export i -> Some i | _ -> None) m.sections
        let s_global = Array.tryPick (fun x -> match x with | Global i -> Some i | _ -> None) m.sections

        let exported_func_names = 
            match s_export with
            | None -> Array.empty
            | Some s ->
                let f (e : ExportItem) =
                    match e.desc with
                    | ExportFunc fidx -> Some (fidx, e.name)
                    | _ -> None
                Array.choose f s.exports

        let get_func_internals sf num_imports =
            let find_exported_func_name fidx =
                Array.tryPick (fun (idx,name) -> if fidx = idx then Some name else None) exported_func_names

            let f i t =
                let fidx = i + num_imports
                let name = find_exported_func_name (FuncIdx (uint32 fidx))
                F_Internal { if_name = name; if_typ = t }
                
            Array.mapi f sf.funcs

        let get_func_imports si =
            (Array.choose (fun i -> match i.desc with | ImportFunc typeidx -> Some (F_Imported {f_m = i.m; f_name = i.name; f_typ = typeidx;}) | _ -> None) si.imports)

        let flookup =
            match (s_import, s_function) with
            | (Some simp, Some sint) ->
                let ia = get_func_imports simp
                let fa = get_func_internals sint ia.Length
                Array.append ia fa
            | (Some simp, None) ->
                get_func_imports simp
            | (None, Some sint) ->
                get_func_internals sint 0
            | (None, None) ->
                Array.empty

        let exported_global_names = 
            match s_export with
            | None -> Array.empty
            | Some s ->
                let f (e : ExportItem) =
                    match e.desc with
                    | ExportGlobal gt -> Some (gt, e.name)
                    | _ -> None
                Array.choose f s.exports

        let get_global_internals sf num_imports =
            let find_exported_global_name fidx =
                Array.tryPick (fun (idx,name) -> if fidx = idx then Some name else None) exported_global_names

            let f i t =
                let fidx = i + num_imports
                let name = find_exported_global_name (GlobalIdx (uint32 fidx))
                G_Internal { ig_name = name; ig_typ = t.globaltype }
                
            Array.mapi f sf.globals

        let get_global_imports si =
            (Array.choose (fun i -> match i.desc with | ImportGlobal gt -> Some (G_Imported {g_m = i.m; g_name = i.name; g_typ = gt;}) | _ -> None) si.imports)

        let glookup =
            match (s_import, s_global) with
            | (Some simp, Some sint) ->
                let ia = get_global_imports simp
                let fa = get_global_internals sint ia.Length
                Array.append ia fa
            | (Some simp, None) ->
                get_global_imports simp
            | (None, Some sint) ->
                get_global_internals sint 0
            | (None, None) ->
                Array.empty

        {
            Import = s_import
            Function = s_function
            Export = s_export
            Global = s_global
            Type = Array.tryPick (fun x -> match x with | Type i -> Some i | _ -> None) m.sections
            Table = Array.tryPick (fun x -> match x with | Table i -> Some i | _ -> None) m.sections
            Memory = Array.tryPick (fun x -> match x with | Memory i -> Some i | _ -> None) m.sections
            Start = Array.tryPick (fun x -> match x with | Start i -> Some i | _ -> None) m.sections
            Element = Array.tryPick (fun x -> match x with | Element i -> Some i | _ -> None) m.sections
            Code = Array.tryPick (fun x -> match x with | Code i -> Some i | _ -> None) m.sections
            Data = Array.tryPick (fun x -> match x with | Data i -> Some i | _ -> None) m.sections
            FuncLookup = flookup
            GlobalLookup = glookup
        }

    let is_function_exported ndx i =
        let fmatch (x : ExportItem) =
            match x.desc with
            | ExportFunc n when n = i -> Some x.name
            | _ -> None

        match ndx.Export with
        | Some s -> match Array.tryPick fmatch s.exports with | Some _ -> true | None -> false
        | None -> false

    let lookup_function ndx i =
        ndx.FuncLookup.[i]

    let get_function_type_by_typeidx ndx typeidx =
        let (TypeIdx i) = typeidx
        match ndx.Type with
        | Some s -> Some (s.types.[int i])
        | None -> None

