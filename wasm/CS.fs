
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
                printf "    "
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

    type LocalNameAndType = {
        name : string;
        typ : ValType;
        }

    let get_function_name fidx f =
        match f with
        | Imported i -> i.f_name // TODO need module/namespace/whatever too
        | Internal i -> 
            match i.if_name with
            | Some s -> s
            | None -> sprintf "func_%d" fidx

    let cs_expr depth ndx (a_locals : LocalNameAndType[]) e =
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
                let depth = cur_depth + idepth - 1
                //prn depth (sprintf "// %s" (stringify_instruction cb op))
                match op with
                | Call (FuncIdx fidx) ->
                    let found = lookup_function ndx (int fidx)
                    let name = get_function_name fidx found
                    let typeidx =
                        match found with
                        | Imported i -> i.f_typ
                        | Internal i -> i.if_typ
                    let ft = 
                        match get_function_type_by_typeidx ndx typeidx with
                        | Some q -> q
                        | None -> failwith "unknown func type"
                    let args =
                        let a = 
                            ft.parms
                            |> Array.map (fun vt -> sprintf "stack.pop_%s()" (cs_valtype vt))
                        System.String.Join(", ", a)
                    let s = sprintf "%s(%s)" name args
                    let s =
                        if ft.result.Length = 0 then
                            s
                        else
                            sprintf "stack.push_%s(%s)" (cs_valtype (ft.result.[0])) s
                    let s = s + ";"
                    prn depth s
                | LocalGet (LocalIdx i) -> 
                    let loc = a_locals.[int i]
                    let typ = cs_valtype loc.typ
                    prn depth (sprintf "stack.push_%s(%s)" typ loc.name)
                | I32Add -> prn depth "stack.push_i32(stack.pop_i32() + stack.pop_i32())"
                | I32Mul -> prn depth "stack.push_i32(stack.pop_i32() * stack.pop_i32())"
                | F64Add -> prn depth "stack.push_f64(stack.pop_f64() + stack.pop_f64())"
                | F64Mul -> prn depth "stack.push_f64(stack.pop_f64() * stack.pop_f64())"
                | Block t -> prn depth "{"
                | End -> prn depth "}"
                | Drop -> prn depth "stack.pop();"
                | I32Const i -> prn depth (sprintf "stack.push_i32(%d);" i)
                | I64Const i -> prn depth (sprintf "stack.push_i64(%d);" i)
                | F32Const f -> prn depth (sprintf "stack.push_f32(%f);" f)
                | F64Const f -> prn depth (sprintf "stack.push_f64(%f);" f)
                | _ -> prn depth (sprintf "// TODO %s" (stringify_instruction cb op))
                idepth <- next_idepth

    let cs_function_item ndx fidx tidx cit =
        prn 1 (sprintf "// func %d" fidx)
        let is_exported = is_function_exported ndx (FuncIdx fidx)

        let found = lookup_function ndx (int fidx)
        let name = get_function_name fidx found

        // TODO need to cleanse the name to be C#-compliant

        let ftype = 
            match get_function_type_by_typeidx ndx tidx with
            | Some ft -> ft
            | _ -> failwith "unknown function type"

        let return_type =
            if ftype.result.Length = 0 then
                "void"
            else
                cs_valtype ftype.result.[0]

        let a_locals =
            let a = System.Collections.Generic.List<LocalNameAndType>()
            let get_name () =
                sprintf "p%d" (a.Count)
            for x in ftype.parms do
                a.Add({ name = get_name(); typ = x})
            for loc in cit.locals do
                // TODO assert count > 0 ?
                for x = 1 to (int loc.count) do
                    a.Add({ name = get_name(); typ = loc.localtype})
            Array.ofSeq a
            
        let str_parms = 
            if a_locals.Length > 0 then
                let a_parm_decls =
                    a_locals.[0..(ftype.parms.Length-1)]
                    |> Array.map (fun pair -> sprintf "%s %s" (cs_valtype pair.typ) pair.name)
                System.String.Join(", ", a_parm_decls)
            else
                ""

        let access = if is_exported then "public" else "private"
        prn 1 (sprintf "%s static %s %s(%s)" access return_type name str_parms)
        prn 1 "{"

        if a_locals.Length > ftype.parms.Length then
            for loc in a_locals.[ftype.parms.Length..] do
                let typ = cs_valtype loc.typ
                prn 2 (sprintf "%s %s = default(%s);" typ loc.name typ)
        cs_expr 2 ndx a_locals cit.expr
        prn 1 "}"

    let cs_function_section ndx sf sc =
        let count_imports = count_function_imports ndx

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

