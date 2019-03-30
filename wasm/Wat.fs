
module wasm.wat

    open wasm.def_basic
    open wasm.def
    open wasm.stringify_args
    open wasm.stringify_instr
    open wasm.def_instr
    open wasm.module_index

    let prn depth s =
        let rec f i =
            if i > 0 then
                printf "  "
                f (i - 1)
                
        f depth
        printfn "%s" s
        
    let wat_valtype vt =
        match vt with
        | I32 -> "i32"
        | I64 -> "i64"
        | F32 -> "f32"
        | F64 -> "f64"

    let cb = {
        // TODO could lookup the func idx and give a name
        stringify_funcidx = fun x -> let (FuncIdx i) = x in sprintf "%d" i

        // TODO give this a name
        stringify_localidx = fun x -> let (LocalIdx i) = x in sprintf "%d" i

        // TODO give this a name
        stringify_globalidx = fun x -> let (GlobalIdx i) = x in sprintf "%d" i

        // TODO give this a name
        stringify_labelidx = fun x -> let (LabelIdx i) = x in sprintf "%d" i

        stringify_resulttype = fun x -> match x with | Some vt -> wat_valtype vt | None -> "TODO"

        stringify_brtable = fun x -> "TODO"
        stringify_memarg = fun x -> sprintf "(align=%d offset=%d)" x.align x.offset
        stringify_callindirect = fun x -> "TODO"
        }

    let wat_custom_section s =
        prn 1 "(custom"
        prn 2 s.name
        prn 2 (sprintf "(%d bytes)" s.data.Length)
        prn 2 ")"

    let wat_limits depth lim =
        match lim with
        | Min min ->
            sprintf "Min: %d" min |> prn depth
        | MinMax (min,max) -> 
            sprintf "MinMax: %d,%d" min max |> prn depth

    let wat_functype depth ft =
        prn 1 "(type"
        prn 2 "(func"
        for t in ft.parms do
            let s = wat_valtype t
            prn 3 (sprintf "(param %s)" s)
        for t in ft.result do
            let s = wat_valtype t
            prn 3 (sprintf "(result %s)" s)
        prn 3 ")"
        prn 2 ")"

    let wat_instruction depth op =
        let s = stringify_instruction cb op
        prn depth s

    let wat_expr depth e =
        let mutable idepth = 1
        for op in e do
            let next_idepth =
                match op with
                | Block _ -> idepth + 1
                | Loop _ -> idepth + 1
                | If _ -> idepth + 1
                | End -> idepth - 1
                | _ -> idepth
            if next_idepth = 0 then
                ()
            else
                wat_instruction (depth + idepth - 1) op
                idepth <- next_idepth

    let wat_elemtype depth t =
        match t with
        | FuncRef -> prn depth "anyfunc"

    let wat_table_item depth t =
        wat_elemtype depth t.elemtype
        wat_limits depth t.limits

    let wat_memory_item depth t =
        prn depth "(memory"
        wat_limits (depth + 1) t.limits
        prn (depth + 1) ")"

    let wat_globaltype depth g =
        let t = wat_valtype g.typ
        sprintf "(%s %s)" (wat_valtype g.typ) (if g.mut then "true" else "false") |> prn depth
        
    let wat_global_item g =
        prn 1 "(global"
        wat_globaltype 2 g.globaltype
        wat_expr 2 g.init
        prn 2 ")"

    let wat_importdesc depth d =
        match d with
        | ImportFunc (TypeIdx i) ->
            sprintf "(func %d)" i |> prn depth
        | ImportTable t ->
            wat_table_item depth t
        | ImportMem t ->
            wat_memory_item depth t
        | ImportGlobal t ->
            wat_globaltype depth t

    let wat_import_item it =
        prn 1 "(import"
        prn 2 it.m
        prn 2 it.name
        wat_importdesc 2 it.desc
        prn 2 ")"

    let wat_local depth loc =
        sprintf "(local %d %s)" loc.n (wat_valtype loc.localtype) |> prn depth

    let wat_function_item ndx i tidx cit =
        prn 1 "(func"
        let (TypeIdx i) = tidx
        sprintf "(type %d)" i |> prn 2
        for loc in cit.locals do
            wat_local 2 loc
        wat_expr 2 cit.expr
        prn 2 ")"

    let wat_exportdesc depth d =
        match d with
        | ExportFunc (FuncIdx i) -> 
            sprintf "(func %d)" i |> prn depth
        | ExportTable (TableIdx i) -> 
            sprintf "(table %d)" i |> prn depth
        | ExportMem (MemIdx i) -> 
            sprintf "(memory %d)" i |> prn depth
        | ExportGlobal (GlobalIdx i) -> 
            sprintf "(global %d)" i |> prn depth

    let wat_export_item (it : ExportItem) =
        prn 1 "(export"
        prn 2 ("\"" + it.name + "\"")
        wat_exportdesc 2 it.desc
        prn 2 ")"

    let wat_element_item it =
        prn 1 "(element"
        let (TableIdx i) = it.tableidx
        sprintf "%d" i |> prn 2
        prn 2 "(offset"
        wat_expr 3 it.offset
        prn 3 ")"
        for x in it.init do
            let (FuncIdx i) = x
            sprintf "%d" i |> prn 2
        prn 2 ")"

    let wat_data_item it =
        prn 1 "(data"
        let (MemIdx i) = it.memidx
        sprintf "%d" i |> prn 2
        prn 2 "(offset"
        wat_expr 3 it.offset
        prn 3 ")"
        prn 2 "TODO data string it.init"
        prn 2 ")"

    let wat_type_section s =
        for it in s.types do
            wat_functype 1 it

    let wat_import_section s =
        for it in s.imports do
            wat_import_item it

    let wat_function_section ndx sf sc =
        let count = sf.funcs.Length
        for i = 0 to (count - 1) do
            wat_function_item ndx i (sf.funcs.[i]) (sc.codes.[i])

    let wat_table_section s =
        for it in s.tables do
            wat_table_item 1 it

    let wat_memory_section s =
        for it in s.mems do
            wat_memory_item 1 it

    let wat_global_section s =
        for it in s.globals do
            wat_global_item it

    let wat_export_section s =
        for it in s.exports do
            wat_export_item it

    let wat_start_section s =
        () // TODO

    let wat_element_section s =
        for it in s.elems do
            wat_element_item it

    let wat_data_section s =
        for it in s.datas do
            wat_data_item it

    let wat_module m =
        prn 0 "(module"

        let ndx = get_module_index m

        match ndx.Type with | Some s -> wat_type_section s | None -> ()
        match ndx.Import with | Some s -> wat_import_section s | None -> ()
        match ndx.Memory with | Some s -> wat_memory_section s | None -> ()
        match ndx.Global with | Some s -> wat_global_section s | None -> ()
        match ndx.Export with | Some s -> wat_export_section s | None -> ()

        match ndx.Data with | Some s -> wat_data_section s | None -> ()

        match ndx.Table with | Some s -> wat_table_section s | None -> ()
        match ndx.Element with | Some s -> wat_element_section s | None -> ()

        match (ndx.Function, ndx.Code) with 
        | (Some sf, Some sc) -> wat_function_section ndx sf sc 
        | _ -> () // TODO error if one but not the other?

        match ndx.Start with | Some s -> wat_start_section s | None -> ()

        prn 1 ")"

