
module wasm.wat

    open wasm.def_basic
    open wasm.def
    open wasm.stringify_args
    open wasm.stringify_instr

    let prn depth s =
        let rec f i =
            if i > 0 then
                printf "  "
                f (i - 1)
                
        f depth
        printfn "%s" s
        
    let cb = {
        stringify_funcidx = fun x -> sprintf "%A" x
        stringify_brtable = fun x -> "TODO"
        stringify_memarg = fun x -> sprintf "(align=%d offset=%d)" x.align x.offset
        stringify_callindirect = fun x -> "TODO"
        }

    let wat_custom_section s =
        prn 1 "(custom"
        prn 2 s.name
        prn 2 (sprintf "(%d bytes)" s.data.Length)
        prn 2 ")"

    let wat_valtype vt =
        match vt with
        | I32 -> "I32"
        | I64 -> "I64"
        | F32 -> "F32"
        | F64 -> "F64"

    let wat_limits depth lim =
        match lim with
        | Min min ->
            sprintf "Min: %d" min |> prn depth
        | MinMax (min,max) -> 
            sprintf "MinMax: %d,%d" min max |> prn depth

    let wat_functype depth ft =
        prn 1 "(type"
        for t in ft.parms do
            let s = wat_valtype t
            prn 2 (sprintf "(param %s)" s)
        for t in ft.result do
            let s = wat_valtype t
            prn 2 (sprintf "(result %s)" s)
        prn 2 ")"

    let wat_instruction depth op =
        let s = stringify_instruction cb op
        prn depth s

    let wat_expr depth e =
        for op in e do
            // TODO keep track of depth changes for block and if etc
            wat_instruction depth op

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

    let wat_typeidx depth i =
        sprintf "%A" i |> prn depth

    let wat_funcidx depth i =
        sprintf "%A" i |> prn depth

    let wat_tableidx depth i =
        sprintf "%A" i |> prn depth

    let wat_memidx depth i =
        sprintf "%A" i |> prn depth

    let wat_globalidx depth i =
        sprintf "%A" i |> prn depth

    let wat_importdesc depth d =
        match d with
        | ImportFunc i ->
            wat_typeidx depth i
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

    let wat_function_item tidx =
        prn 1 "(func"
        wat_typeidx 2 tidx
        prn 2 ")"

    let wat_exportdesc depth d =
        match d with
        | ExportFunc i -> 
            wat_funcidx depth i
        | ExportTable i -> 
            wat_tableidx depth i
        | ExportMem i -> 
            wat_memidx depth i
        | ExportGlobal i -> 
            wat_globalidx depth i

    let wat_export_item (it : ExportItem) =
        prn 1 "(element"
        prn 2 it.name
        wat_exportdesc 2 it.desc
        prn 2 ")"

    let wat_element_item it =
        prn 1 "(element"
        wat_tableidx 2 it.tableidx
        wat_expr 2 it.offset
        for x in it.init do
            wat_funcidx 2 it
        prn 2 ")"

    let wat_local depth loc =
        sprintf "(local %d %s)" loc.n (wat_valtype loc.localtype) |> prn depth

    let wat_code_item it =
        prn 1 "(code"
        for loc in it.locals do
            wat_local 2 loc
        wat_expr 2 it.expr
        prn 2 ")"

    let wat_data_item it =
        prn 1 "(data"
        wat_memidx 2 it.memidx
        wat_expr 2 it.offset
        // TODO wat_blob it.init
        prn 2 ")"

    let wat_type_section s =
        for it in s.types do
            wat_functype 1 it

    let wat_import_section s =
        for it in s.imports do
            wat_import_item it

    let wat_function_section s =
        for it in s.funcs do
            wat_function_item it

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

    let wat_code_section s =
        for it in s.codes do
            wat_code_item it

    let wat_data_section s =
        for it in s.datas do
            wat_data_item it

    let wat_section s =
        match s with
        | Custom s -> wat_custom_section s
        | Type s -> wat_type_section s
        | Import s -> wat_import_section s
        | Function s -> wat_function_section s
        | Table s -> wat_table_section s
        | Memory s -> wat_memory_section s
        | Global s -> wat_global_section s
        | Export s -> wat_export_section s
        | Start s -> wat_start_section s
        | Element s -> wat_element_section s
        | Code s -> wat_code_section s
        | Data s -> wat_data_section s

    let wat_module m =
        prn 0 "(module"
        for s in m.sections do
            wat_section s
        prn 1 ")"

