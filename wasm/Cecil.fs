
module wasm.cecil

    open Mono.Cecil
    open Mono.Cecil.Cil

    open wasm.def_basic
    open wasm.def
    open wasm.def_instr
    open wasm.module_index

    type BasicTypes = {
        typ_void : TypeReference
        typ_i32 : TypeReference
        typ_i64 : TypeReference
        typ_f32 : TypeReference
        typ_f64 : TypeReference
        }

    let cecil_valtype bt vt =
        match vt with
        | I32 -> bt.typ_i32
        | I64 -> bt.typ_i64
        | F32 -> bt.typ_f32
        | F64 -> bt.typ_f64

    type ParamInfo = {
        P_typ : ValType;
        P_def : ParameterDefinition;
        }

    type LocalInfo = {
        L_typ : ValType;
        L_def : VariableDefinition;
        }

    type ParamOrVar =
        | PV_Param of ParamInfo
        | PV_Var of LocalInfo

    type MethodRefImported = {
        func : ImportedFunc
        // TODO need a call object
        }

    type MethodRefInternal = {
        func : InternalFunc
        method : MethodDefinition
        }

    type MethodStuff = 
        | M_Imported of MethodRefImported
        | M_Internal of MethodRefInternal

    let get_function_name fidx f =
        match f with
        | F_Imported i -> sprintf "%s.%s" i.f_m i.f_name
        | F_Internal i -> 
            match i.if_name with
            | Some s -> s
            | None -> sprintf "func_%d" fidx

    type CodeBlock =
        | CB_Block of Mono.Cecil.Cil.Instruction
        | CB_Loop of Mono.Cecil.Cil.Instruction
        | CB_If of Mono.Cecil.Cil.Instruction
        | CB_Else of Mono.Cecil.Cil.Instruction

    let cecil_expr (il : ILProcessor) bt (a_methods : MethodStuff[]) (a_locals : ParamOrVar[]) e =
        let blocks = System.Collections.Generic.Stack<CodeBlock>()
        let lab_end = il.Create(OpCodes.Nop)
        for op in e do
            match op with
            | Nop -> il.Append(il.Create(OpCodes.Nop))
            | Block t -> 
                let lab = il.Create(OpCodes.Nop)
                let blk = CB_Block lab
                blocks.Push(blk)
            | Loop t -> 
                let lab = il.Create(OpCodes.Nop)
                let blk = CB_Loop lab
                blocks.Push(blk)
                il.Append(lab)
            | If t -> 
                let lab = il.Create(OpCodes.Nop)
                let blk = CB_If lab
                blocks.Push(blk)
            | Else -> 
                // first, end the if block
                match blocks.Pop() with
                | CB_If lab -> il.Append(lab)
                | _ -> failwith "bad nest"

                let lab = il.Create(OpCodes.Nop)
                let blk = CB_Else lab
                blocks.Push(blk)
            | End -> 
                if blocks.Count = 0 then
                    il.Append(lab_end)
                else
                    let blk = blocks.Pop()
                    match blk with
                    | CB_Block lab -> il.Append(lab)
                    | CB_Loop _ -> () // loop label was at the top
                    | CB_If lab -> il.Append(lab)
                    | CB_Else lab -> il.Append(lab)
            | Return ->
                il.Append(il.Create(OpCodes.Br, lab_end))
            | Br (LabelIdx i) ->
                let a = blocks.ToArray()
                let blk = a.[int i]
                let lab =
                    match blk with
                    | CB_Block s -> s
                    | CB_Loop s -> s
                    | CB_If s -> s
                    | CB_Else s -> s
                il.Append(il.Create(OpCodes.Br, lab))
            | BrIf (LabelIdx i) ->
                let a = blocks.ToArray()
                let blk = a.[int i]
                let lab =
                    match blk with
                    | CB_Block s -> s
                    | CB_Loop s -> s
                    | CB_If s -> s
                    | CB_Else s -> s
                il.Append(il.Create(OpCodes.Brtrue, lab))
            | Call (FuncIdx fidx) ->
                let fn = a_methods.[int fidx]
                match fn with
                | M_Imported mf ->
                    () // TODO
                | M_Internal mf ->
                    il.Append(il.Create(OpCodes.Call, mf.method))
            | LocalTee (LocalIdx i) -> 
                il.Append(il.Create(OpCodes.Dup))
                let loc = a_locals.[int i]
                match loc with
                | PV_Param { P_def = n } -> il.Append(il.Create(OpCodes.Starg, n))
                | PV_Var { L_def = n } -> il.Append(il.Create(OpCodes.Stloc, n))

            | LocalSet (LocalIdx i) -> 
                let loc = a_locals.[int i]
                match loc with
                | PV_Param { P_def = n } -> il.Append(il.Create(OpCodes.Starg, n))
                | PV_Var { L_def = n } -> il.Append(il.Create(OpCodes.Stloc, n))

            | LocalGet (LocalIdx i) -> 
                let loc = a_locals.[int i]
                match loc with
                | PV_Param { P_def = n } -> il.Append(il.Create(OpCodes.Ldarg, n))
                | PV_Var { L_def = n } -> il.Append(il.Create(OpCodes.Ldloc, n))

            | I32Const i -> il.Append(il.Create(OpCodes.Ldc_I4, i))
            | I64Const i -> il.Append(il.Create(OpCodes.Ldc_I8, i))
            | F32Const i -> il.Append(il.Create(OpCodes.Ldc_R4, i))
            | F64Const i -> il.Append(il.Create(OpCodes.Ldc_R8, i))

            | I32Add | I64Add | F32Add | F64Add -> il.Append(il.Create(OpCodes.Add))
            | I32Mul | I64Mul | F32Mul | F64Mul -> il.Append(il.Create(OpCodes.Mul))
            | I32Sub | I64Sub | F32Sub | F64Sub -> il.Append(il.Create(OpCodes.Sub))
            | I32DivS | I64DivS | F32Div | F64Div -> il.Append(il.Create(OpCodes.Div))
            | I32DivU | I64DivU -> il.Append(il.Create(OpCodes.Div_Un))

            | I32Eqz ->
                il.Append(il.Create(OpCodes.Ldc_I4_0))
                il.Append(il.Create(OpCodes.Ceq))
                
            | I64Eqz ->
                il.Append(il.Create(OpCodes.Ldc_I8, 0L))
                il.Append(il.Create(OpCodes.Ceq))
                
            | I32LtS | I64LtS | F32Lt | F64Lt -> il.Append(il.Create(OpCodes.Clt))
            | I32LtU | I64LtU -> il.Append(il.Create(OpCodes.Clt_Un))

            | F32Neg -> il.Append(il.Create(OpCodes.Neg))
            | F64Neg -> il.Append(il.Create(OpCodes.Neg))

            | I32Eq | I64Eq | F32Eq | F64Eq -> il.Append(il.Create(OpCodes.Ceq))

            | I32Ne | I64Ne | F32Ne | F64Ne -> 
                il.Append(il.Create(OpCodes.Ceq))
                il.Append(il.Create(OpCodes.Ldc_I4_0))
                il.Append(il.Create(OpCodes.Ceq))

            | I32LeS | I64LeS ->
                il.Append(il.Create(OpCodes.Cgt))
                il.Append(il.Create(OpCodes.Ldc_I4_0))
                il.Append(il.Create(OpCodes.Ceq))

            | F32Le | F64Le ->
                il.Append(il.Create(OpCodes.Cgt_Un))
                il.Append(il.Create(OpCodes.Ldc_I4_0))
                il.Append(il.Create(OpCodes.Ceq))

            | Drop -> il.Append(il.Create(OpCodes.Pop))

            // TODO when all the cases are done, the following line will be removed
            | _ -> printfn "TODO: %A" op

    let create_method fi fidx bt =
        let name = 
            match fi.if_name with
            | Some s -> s
            | None -> sprintf "func_%d" fidx

        let return_type =
            match fi.if_typ.result.Length with
            | 0 -> bt.typ_void
            | 1 -> cecil_valtype bt (fi.if_typ.result.[0])
            | _ -> failwith "not implemented"

        let access = MethodAttributes.Public // TODO fix

        let method = 
            new MethodDefinition(
                name,
                access ||| Mono.Cecil.MethodAttributes.Static, 
                return_type
                )

        method

    let gen_function_code a_methods (mi : MethodRefInternal) bt =
        let a_locals =
            let a = System.Collections.Generic.List<ParamOrVar>()
            let get_name () =
                sprintf "p%d" (a.Count)
            for x in mi.func.if_typ.parms do
                let typ = cecil_valtype bt x
                let name = get_name()
                let def = new ParameterDefinition(name, ParameterAttributes.None, typ)
                a.Add(PV_Param { P_def = def; P_typ = x; })
            for loc in mi.func.code.locals do
                // TODO assert count > 0 ?
                for x = 1 to (int loc.count) do
                    let typ = cecil_valtype bt loc.localtype
                    let def = new VariableDefinition(typ)
                    a.Add(PV_Var { L_def = def; L_typ = loc.localtype })
            Array.ofSeq a
            
        mi.method.Body.InitLocals <- true
        for pair in a_locals do
            match pair with
            | PV_Param { P_def = def } -> mi.method.Parameters.Add(def)
            | PV_Var { L_def = def } -> mi.method.Body.Variables.Add(def)

        let il = mi.method.Body.GetILProcessor()
        cecil_expr il bt a_methods a_locals mi.func.code.expr
        il.Append(il.Create(OpCodes.Ret))

    let cecil_function_section ndx sf sc (container : TypeDefinition) bt =
        let count_imports = count_function_imports ndx

        let prep_func i fi =
            match fi with
            | F_Imported s ->
                // TODO
                M_Imported { MethodRefImported.func = s }
            | F_Internal q ->
                let method = create_method q (count_imports + i) bt
                container.Methods.Add(method)
                M_Internal { func = q; method = method; }

        let a_methods = Array.mapi prep_func ndx.FuncLookup

        for m in a_methods do
            match m with
            | M_Internal mi -> gen_function_code a_methods mi bt
            | M_Imported _ -> ()

    let cecil_module m =
        let assembly = 
            AssemblyDefinition.CreateAssembly(
                new AssemblyNameDefinition(
                    "HelloWorld", 
                    new System.Version(1, 0, 0, 0)
                    ), 
                "HelloWorld", 
                ModuleKind.Console
                )

        let main_module = assembly.MainModule

        let bt = 
            {
                typ_i32 = main_module.TypeSystem.Int32
                typ_i64 = main_module.TypeSystem.Int64
                typ_f32 = main_module.TypeSystem.Single
                typ_f64 = main_module.TypeSystem.Double
                typ_void = main_module.TypeSystem.Void
            }

        let container = 
            new TypeDefinition(
                "HelloWorld", 
                "Program",
                TypeAttributes.Class ||| TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed, 
                main_module.TypeSystem.Object
                )

        main_module.Types.Add(container);

        let ndx = get_module_index m

        match (ndx.Function, ndx.Code) with 
        | (Some sf, Some sc) -> cecil_function_section ndx sf sc container bt
        | _ -> () // TODO error if one but not the other?

        assembly.Write("hello.dll");

