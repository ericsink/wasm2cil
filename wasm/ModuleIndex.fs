
module wasm.module_index

    open wasm.def
    open wasm.def_basic

    type ImportedTable = {
        m : string
        name : string
        tbl : TableType
        }

    type ImportedFunc = {
        m : string
        name : string
        typ : FuncType
        }

    type InternalFunc = {
        name : string option;
        typ : FuncType;
        code : CodeItem
        exported : bool
        idx : uint32
        }

    type FuncLookupItem =
        | ImportedFunc of ImportedFunc
        | InternalFunc of InternalFunc

    type ImportedMemory = {
        m : string
        name : string
        mem : MemType
        }

    type ImportedGlobal = {
        m : string
        name : string
        typ : GlobalType
        }

    type InternalGlobal = {
        name : string option;
        item : GlobalItem;
        exported : bool
        }

    type GlobalLookupItem =
        | ImportedGlobal of ImportedGlobal
        | InternalGlobal of InternalGlobal

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
        MemoryImport : ImportedMemory option
        TableImport : ImportedTable option
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
        let s_code = Array.tryPick (fun x -> match x with | Code i -> Some i | _ -> None) m.sections
        let s_type = Array.tryPick (fun x -> match x with | Type i -> Some i | _ -> None) m.sections

        let exported_func_names = 
            match s_export with
            | None -> Array.empty
            | Some s ->
                let f (e : ExportItem) =
                    match e.desc with
                    | ExportFunc fidx -> Some (fidx, e.name)
                    | _ -> None
                Array.choose f s.exports

        let get_func_internals sf st sc num_func_imports =
            let find_exported_func_name fidx =
                Array.tryPick (fun (idx,name) -> if fidx = idx then Some name else None) exported_func_names

            let f i t =
                let fidx = i + num_func_imports
                let name = find_exported_func_name (FuncIdx (uint32 fidx))
                let (TypeIdx i_type) = t
                let exported =
                    match name with
                    | Some _ -> true
                    | None -> false
                InternalFunc { name = name; typ = st.types.[int i_type]; code = sc.codes.[i]; exported = exported; idx = uint32 fidx }
                
            Array.mapi f sf.funcs

        let get_func_imports si st =
            (Array.choose (fun i -> match i.desc with | ImportFunc (TypeIdx i_type) -> Some (ImportedFunc {m = i.m; name = i.name; typ = st.types.[int i_type];}) | _ -> None) si.imports)

        // TODO problem with the match cases below.  the issue
        // is that the spec says all sections are optional, but
        // in some cases, if a certain section is present, then
        // another section is now required.
        let flookup =
(*
            printfn "type %A" s_type
            printfn "import %A" s_import
            printfn "function %A" s_function
            printfn "code %A" s_code
*)
            match (s_type, s_import, s_function, s_code) with
            | (Some st, Some simp, Some sint, Some sc) ->
                let ia = get_func_imports simp st
                let fa = get_func_internals sint st sc ia.Length
                Array.append ia fa
            | (Some st, Some simp, None, None) ->
                get_func_imports simp st
            | (Some st, None, Some sint, Some sc) ->
                get_func_internals sint st sc 0
            | (None, Some si, None, None) ->
                Array.empty
            | (None, None, None, None) ->
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

        let get_global_internals sf num_global_imports =
            let find_exported_global_name fidx =
                Array.tryPick (fun (idx,name) -> if fidx = idx then Some name else None) exported_global_names

            let f i t =
                let fidx = i + num_global_imports
                let name = find_exported_global_name (GlobalIdx (uint32 fidx))
                let exported =
                    match name with
                    | Some _ -> true
                    | None -> false
                InternalGlobal { name = name; item = t; exported = exported }
                
            Array.mapi f sf.globals

        let get_global_imports si =
            (Array.choose (fun i -> match i.desc with | ImportGlobal gt -> Some (ImportedGlobal {m = i.m; name = i.name; typ = gt;}) | _ -> None) si.imports)

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

        let memory_import =
            let a = 
                match s_import with
                | Some si ->
                    (Array.choose (fun i -> match i.desc with | ImportMem gt -> Some {m = i.m; name = i.name; mem = gt;} | _ -> None) si.imports)
                | None -> [| |]
            if a.Length = 0 then
                None
            else if a.Length = 1 then
                Some (a.[0])
            else
                failwith "not supported"

        let table_import =
            let a = 
                match s_import with
                | Some si ->
                    (Array.choose (fun i -> match i.desc with | ImportTable gt -> Some {m = i.m; name = i.name; tbl = gt;} | _ -> None) si.imports)
                | None -> [| |]
            if a.Length = 0 then
                None
            else if a.Length = 1 then
                Some (a.[0])
            else
                failwith "not supported"

        {
            Import = s_import
            Function = s_function
            Export = s_export
            Global = s_global
            Code = s_code
            Type = s_type
            Table = Array.tryPick (fun x -> match x with | Table i -> Some i | _ -> None) m.sections
            Memory = Array.tryPick (fun x -> match x with | Memory i -> Some i | _ -> None) m.sections
            Start = Array.tryPick (fun x -> match x with | Start i -> Some i | _ -> None) m.sections
            Element = Array.tryPick (fun x -> match x with | Element i -> Some i | _ -> None) m.sections
            Data = Array.tryPick (fun x -> match x with | Data i -> Some i | _ -> None) m.sections
            FuncLookup = flookup
            GlobalLookup = glookup
            MemoryImport = memory_import
            TableImport = table_import
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

