
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
        | I32 -> "i32"
        | I64 -> "i64"
        | F32 -> "f32"
        | F64 -> "f64"

    let cb = {
        stringify_funcidx = fun x -> let (FuncIdx i) = x in sprintf "%d" i

        stringify_localidx = fun x -> let (LocalIdx i) = x in sprintf "%d" i

        stringify_globalidx = fun x -> let (GlobalIdx i) = x in sprintf "%d" i

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
        | Imported i -> sprintf "%s.%s" i.f_m i.f_name
        | Internal i -> 
            match i.if_name with
            | Some s -> s
            | None -> sprintf "func_%d" fidx

    type CodeBlock =
        | CB_Block of string
        | CB_Loop of string
        | CB_If of string
        | CB_Else of string

    let cs_expr depth ndx (a_locals : LocalNameAndType[]) e =
        let mutable next_lab = 0
        let get_label kind =
            let t = sprintf "%s_%d" kind next_lab
            next_lab <- next_lab + 1
            t
        let mutable next_tmp = 0
        let get_tmp vt =
            let t = sprintf "tmp_%s_%d" (cs_valtype vt) next_tmp
            next_tmp <- next_tmp + 1
            t
        let blocks = System.Collections.Generic.Stack<CodeBlock>()
        for op in e do
            let depth = depth + blocks.Count
            //prn depth (sprintf "// %s" (stringify_instruction cb op))
            match op with
            | Block t -> 
                let lab = get_label "block"
                let blk = CB_Block lab
                blocks.Push(blk)
                prn depth "{"
            | Loop t -> 
                let lab = get_label "loop"
                let blk = CB_Loop lab
                blocks.Push(blk)
                prn depth "{"
                prn (depth + 1) (sprintf "%s:" lab)
            | If t -> 
                let lab = get_label "if"
                let blk = CB_If lab
                blocks.Push(blk)
                prn depth "{"
            | Else -> 
                // first, end the if block
                let blk_if = blocks.Pop()
                let lab_if =
                    match blk_if with
                    | CB_If s -> s
                    | _ -> failwith "bad nest"
                prn depth (sprintf "%s: ;" lab_if)
                prn (depth - 1) "}"

                let lab = get_label "else"
                let blk = CB_Else lab
                blocks.Push(blk)
                prn (depth - 1) "{"
            | End -> 
                if blocks.Count = 0 then
                    prn depth "end: ;"
                else
                    let blk = blocks.Pop()
                    match blk with
                    | CB_Block s -> prn depth (sprintf "%s: ;" s)
                    | CB_Loop s -> () // loop label was at the top
                    | CB_If s -> prn depth (sprintf "%s: ;" s)
                    | CB_Else s -> prn depth (sprintf "%s: ;" s)
                    prn (depth - 1) "}"
            | Return ->
                prn depth "goto end;"
            | Br (LabelIdx i) ->
                let a = blocks.ToArray()
                let blk = a.[int i]
                let lab =
                    match blk with
                    | CB_Block s -> s
                    | CB_Loop s -> s
                    | CB_If s -> s
                    | CB_Else s -> s
                prn depth (sprintf "goto %s;" lab)
            | BrIf (LabelIdx i) ->
                let a = blocks.ToArray()
                let blk = a.[int i]
                let lab =
                    match blk with
                    | CB_Block s -> s
                    | CB_Loop s -> s
                    | CB_If s -> s
                    | CB_Else s -> s
                prn depth (sprintf "if (0 != stack.pop_i32()) goto %s;" lab)
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
                let fix_return call =
                    match ft.result.Length with
                    | 0 -> call + ";"
                    | 1 -> sprintf "stack.push_%s(%s);" (cs_valtype (ft.result.[0])) call
                    | _ -> failwith "not implemented"
                match ft.parms.Length with
                | 0 -> 
                    let call = sprintf "%s()" name
                    let s = fix_return call
                    prn depth s
                | 1 -> 
                    let call = sprintf "%s(stack.pop_%s())" name (cs_valtype (ft.parms.[0]))
                    let s = fix_return call
                    prn depth s
                | _ ->
                    prn depth "{"
                    let args =
                        let names = Array.map (fun t -> get_tmp t) ft.parms
                        // need to pop arguments in reverse order
                        for i = (names.Length - 1) downto 0 do
                            prn (depth + 1) (sprintf "var %s = stack.pop_%s();" (names.[i]) (cs_valtype (ft.parms.[i])) )
                        System.String.Join(", ", names)
                    let call = sprintf "%s(%s)" name args
                    let s = fix_return call
                    prn (depth + 1) s
                    prn depth "}"
            | LocalGet (LocalIdx i) -> 
                let loc = a_locals.[int i]
                let typ = cs_valtype loc.typ
                prn depth (sprintf "stack.push_%s(%s);" typ loc.name)
            | LocalSet (LocalIdx i) -> 
                let loc = a_locals.[int i]
                let typ = cs_valtype loc.typ
                prn depth (sprintf "%s = stack.pop_%s();" loc.name typ)
            | LocalTee (LocalIdx i) -> 
                let loc = a_locals.[int i]
                let typ = cs_valtype loc.typ
                prn depth (sprintf "%s = stack.peek_%s();" loc.name typ)
            | I32Sub -> 
                prn depth "{"
                prn (depth + 1) "var t = stack.pop_i32();"
                prn (depth + 1) "stack.push_i32(stack.pop_i32() - t);"
                prn depth "}"
            | I32LtS -> 
                prn depth "{"
                prn (depth + 1) "var t = stack.pop_i32();"
                prn (depth + 1) "stack.push_i32_bool(stack.pop_i32() < t);"
                prn depth "}"
            | I32Add -> prn depth "stack.push_i32(stack.pop_i32() + stack.pop_i32());"
            | I32Mul -> prn depth "stack.push_i32(stack.pop_i32() * stack.pop_i32());"
            | I32Eq -> prn depth "stack.push_i32_bool(stack.pop_i32() == stack.pop_i32());"
            | I32Ne -> prn depth "stack.push_i32_bool(stack.pop_i32() != stack.pop_i32());"
            | F64Add -> prn depth "stack.push_f64(stack.pop_f64() + stack.pop_f64());"
            | F64Mul -> prn depth "stack.push_f64(stack.pop_f64() * stack.pop_f64());"
            | I32Const i -> prn depth (sprintf "stack.push_i32(%d);" i)
            | I64Const i -> prn depth (sprintf "stack.push_i64(%d);" i)
            | F32Const f -> prn depth (sprintf "stack.push_f32(%f);" f)
            | F64Const f -> prn depth (sprintf "stack.push_f64(%f);" f)
            | Drop -> prn depth "stack.pop();"
            | _ -> prn depth (sprintf "// TODO %s" (stringify_instruction cb op))

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
            match ftype.result.Length with
            | 0 -> "void"
            | 1 -> cs_valtype ftype.result.[0]
            | _ -> failwith "not implemented"

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
        prn 2 "var stack = new WasmStack();"

        if a_locals.Length > ftype.parms.Length then
            for loc in a_locals.[ftype.parms.Length..] do
                let typ = cs_valtype loc.typ
                prn 2 (sprintf "%s %s = default(%s);" typ loc.name typ)
        cs_expr 2 ndx a_locals cit.expr
        match ftype.result.Length with
        | 0 -> ()
        | 1 -> 
            let vt = cs_valtype (ftype.result.[0])
            prn 2 (sprintf "return stack.pop_%s();" vt)
        | _ -> failwith "not implemented"
        prn 1 "}"

    let cs_function_section ndx sf sc =
        let count_imports = count_function_imports ndx

        for i = 0 to (sf.funcs.Length - 1) do
            cs_function_item ndx (uint32 (i + count_imports)) (sf.funcs.[i]) (sc.codes.[i])

    let cs_module m =
        prn 0 "using i32 = System.Int32;"
        prn 0 "using i64 = System.Int64;"
        prn 0 "using f32 = System.Single;"
        prn 0 "using f64 = System.Double;"

        // TODO name of the static class
        prn 0 "public static class foo"
        prn 0 "{"

        let ndx = get_module_index m

        match (ndx.Function, ndx.Code) with 
        | (Some sf, Some sc) -> cs_function_section ndx sf sc 
        | _ -> () // TODO error if one but not the other?

        prn 0 "}"

