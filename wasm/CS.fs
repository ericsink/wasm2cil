
module wasm.cs

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
        
    let cs_valtype vt =
        match vt with
        | I32 -> "int32"
        | I64 -> "int64"
        | F32 -> "float"
        | F64 -> "double"

    let cb = {
        // TODO could lookup the func idx and give a name, but
        // we would need the ModuleIndex here
        stringify_funcidx = fun x -> let (FuncIdx i) = x in sprintf "%d" i

        stringify_localidx = fun x -> let (LocalIdx i) = x in sprintf "p%d" i

        // TODO give this a name
        stringify_globalidx = fun x -> let (GlobalIdx i) = x in sprintf "%d" i

        // TODO give this a name
        stringify_labelidx = fun x -> let (LabelIdx i) = x in sprintf "%d" i

        stringify_resulttype = fun x -> match x with | Some vt -> cs_valtype vt | None -> "void"

        stringify_brtable = fun x -> "TODO"
        stringify_memarg = fun x -> sprintf "(align=%d offset=%d)" x.align x.offset
        stringify_callindirect = fun x -> "TODO"
        }

    let cs_instruction depth op =
        let s = stringify_instruction cb op
        prn depth s

    let cs_expr depth e =
        let mutable idepth = 1
        for op in e do
            let (cur_depth,next_idepth) =
                match op with
                | Block _ -> (depth,idepth + 1)
                | Loop _ -> (depth,idepth + 1)
                | If _ -> (depth,idepth + 1)
                | End -> (depth - 1, idepth - 1)
                | Else -> (depth - 1, idepth)
                | _ -> (depth,idepth)
            if next_idepth = 0 then
                ()
            else
                cs_instruction (cur_depth + idepth - 1) op
                idepth <- next_idepth

    let cs_function_item ndx i tidx cit =
        prn 1 (sprintf "// func %d" i)
        let is_exported = is_function_exported ndx (FuncIdx i)

        let name = 
            match get_function_name ndx (FuncIdx i) with
            | Some s -> s
            | None -> sprintf "func_%d" i

        // TODO need to cleanse the name to be C#-compliant

        let ftype = 
            match get_function_type ndx tidx with
            | Some ft -> ft
            | _ -> failwith "unknown function type"

        let return_type =
            if ftype.result.Length = 0 then
                "void"
            else
                cs_valtype ftype.result.[0]

        let a_parm_types = 
            ftype.parms
            |> Array.map cs_valtype

        let a_parm_names = 
            ftype.parms
            |> Array.mapi (fun i x -> sprintf "p%d" i)

        let a_parm_decls =
            Array.zip a_parm_types a_parm_names
            |> Array.map (fun (typ,name) -> sprintf "%s %s" typ name)

        let str_parms = System.String.Join(", ", a_parm_decls)

        let access = if is_exported then "public" else "private"
        prn 1 (sprintf "%s static %s %s(%s)" access return_type name str_parms)
        prn 1 "{"

        let a_local_names = 
            cit.locals
            |> Array.mapi (fun i x -> sprintf "p%d" (i + ftype.parms.Length))

        let mutable cur_local_num = ftype.parms.Length
        for loc in cit.locals do
            // TODO assert count > 0 ?
            let typ = cs_valtype loc.localtype
            for x = 1 to (int loc.count) do
                let name = sprintf "p%d" cur_local_num
                cur_local_num <- cur_local_num + 1
                // TODO how are locals initialized?
                prn 2 (sprintf "%s %s;" typ name)
        cs_expr 2 cit.expr
        prn 1 "}"

    let cs_function_section ndx sf sc =
        let count_imports =
            match ndx.Import with
            | Some s -> s.imports.Length
            | None -> 0

        for i = 0 to (sf.funcs.Length - 1) do
            cs_function_item ndx (uint32 (i + count_imports)) (sf.funcs.[i]) (sc.codes.[i])

    let cs_module m =
        // TODO name of the static class
        prn 0 "public static class foo"
        prn 0 "{"

        let ndx = get_module_index m

        match (ndx.Function, ndx.Code) with 
        | (Some sf, Some sc) -> cs_function_section ndx sf sc 
        | _ -> () // TODO error if one but not the other?

        prn 0 "}"

