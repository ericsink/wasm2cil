
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
        typ_intptr : TypeReference
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

    type GlobalRefImported = {
        glob : ImportedGlobal
        // TODO need something here
        }

    type GlobalRefInternal = {
        glob : InternalGlobal
        field : FieldDefinition
        }

    type GlobalStuff = 
        | GS_Imported of GlobalRefImported
        | GS_Internal of GlobalRefInternal

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

    type GenContext = {
        md : ModuleDefinition
        mem : FieldDefinition
        tbl_lookup : MethodDefinition
        a_globals : GlobalStuff[]
        a_methods : MethodStuff[]
        bt : BasicTypes
        }

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

    type Temps = {
        tmp_i32 : VariableDefinition
        tmp_i64 : VariableDefinition
        tmp_f32 : VariableDefinition
        tmp_f64 : VariableDefinition
        }

    let prep_tmps bt (method : MethodDefinition) =
        // TODO er, let's not declare locals we don't need
        let tmps = 
            {
                tmp_i32 = new VariableDefinition(bt.typ_i32)
                tmp_i64 = new VariableDefinition(bt.typ_i64)
                tmp_f32 = new VariableDefinition(bt.typ_f32)
                tmp_f64 = new VariableDefinition(bt.typ_f64)
            }
        method.Body.Variables.Add(tmps.tmp_i32)
        method.Body.Variables.Add(tmps.tmp_i64)
        method.Body.Variables.Add(tmps.tmp_f32)
        method.Body.Variables.Add(tmps.tmp_f64)
        tmps

    let cecil_expr (il: ILProcessor) ctx tmps (a_locals : ParamOrVar[]) e =
        let tmp_i32 = tmps.tmp_i32
        let tmp_i64 = tmps.tmp_i64
        let tmp_f32 = tmps.tmp_f32
        let tmp_f64 = tmps.tmp_f64
        let blocks = System.Collections.Generic.Stack<CodeBlock>()
        let lab_end = il.Create(OpCodes.Nop)

        let get_label (a :CodeBlock[]) i =
            let blk = a.[int i]
            let lab =
                match blk with
                | CB_Block s -> s
                | CB_Loop s -> s
                | CB_If s -> s
                | CB_Else s -> s
            lab

        let prep_addr m =
            // TODO what about m ?
            il.Append(il.Create(OpCodes.Ldfld, ctx.mem))
            il.Append(il.Create(OpCodes.Add))

        let load m op =
            prep_addr m
            il.Append(il.Create(op))

        let store m op (tmp : VariableDefinition) =
            il.Append(il.Create(OpCodes.Stloc, tmp)) // pop v into tmp
            prep_addr m
            il.Append(il.Create(OpCodes.Ldloc, tmp)) // put v back
            il.Append(il.Create(op))

        let todo q =
            printfn "TODO: %A" q
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
                    printfn "END LABEL"
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
                let lab = get_label a i
                il.Append(il.Create(OpCodes.Br, lab))
            | BrIf (LabelIdx i) ->
                let a = blocks.ToArray()
                let lab = get_label a i
                il.Append(il.Create(OpCodes.Brtrue, lab))
            | BrTable m ->
                let a = blocks.ToArray()
                let q = Array.map (fun i -> get_label a i) m.v
                il.Append(il.Create(OpCodes.Switch, q))
                let lab = get_label a m.other
                il.Append(il.Create(OpCodes.Br, lab))

            | Call (FuncIdx fidx) ->
                let fn = ctx.a_methods.[int fidx]
                match fn with
                | M_Imported mf ->
                    printfn "TODO: Call %A" mf
                | M_Internal mf ->
                    il.Append(il.Create(OpCodes.Call, mf.method))

            | CallIndirect _ ->
                il.Append(il.Create(OpCodes.Call, ctx.tbl_lookup))
                il.Append(il.Create(OpCodes.Calli))

            | GlobalGet (GlobalIdx idx) ->
                let g = ctx.a_globals.[int idx]
                match g with
                | GS_Imported mf ->
                    printfn "TODO: get global %A" mf
                | GS_Internal mf ->
                    il.Append(il.Create(OpCodes.Ldfld, mf.field))

            | GlobalSet (GlobalIdx idx) ->
                let g = ctx.a_globals.[int idx]
                match g with
                | GS_Imported mf ->
                    printfn "TODO: set global %A" mf
                | GS_Internal mf ->
                    il.Append(il.Create(OpCodes.Stfld, mf.field))

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

            | I32Load m -> load m OpCodes.Ldind_I4
            | I64Load m -> load m OpCodes.Ldind_I8
            | F32Load m -> load m OpCodes.Ldind_R4
            | F64Load m -> load m OpCodes.Ldind_R8
            | I32Load8S m -> load m OpCodes.Ldind_I1
            | I32Load8U m -> load m OpCodes.Ldind_U1
            | I32Load16S m -> load m OpCodes.Ldind_I2
            | I32Load16U m -> load m OpCodes.Ldind_U2
            | I64Load8S m -> load m OpCodes.Ldind_I1
            | I64Load8U m -> load m OpCodes.Ldind_U1
            | I64Load16S m -> load m OpCodes.Ldind_I2
            | I64Load16U m -> load m OpCodes.Ldind_U2
            | I64Load32S m -> load m OpCodes.Ldind_I4
            | I64Load32U m -> load m OpCodes.Ldind_U4

            | I32Store m -> store m OpCodes.Stind_I4 tmp_i32
            | I64Store m -> store m OpCodes.Stind_I8 tmp_i64
            | F32Store m -> store m OpCodes.Stind_R4 tmp_f32
            | F64Store m -> store m OpCodes.Stind_R8 tmp_f64
            | I32Store8 m -> store m OpCodes.Stind_I1 tmp_i32
            | I32Store16 m -> store m OpCodes.Stind_I2 tmp_i32
            | I64Store8 m -> store m OpCodes.Stind_I1 tmp_i64
            | I64Store16 m -> store m OpCodes.Stind_I2 tmp_i64
            | I64Store32 m -> store m OpCodes.Stind_I4 tmp_i64

            | I32Const i -> il.Append(il.Create(OpCodes.Ldc_I4, i))
            | I64Const i -> il.Append(il.Create(OpCodes.Ldc_I8, i))
            | F32Const i -> il.Append(il.Create(OpCodes.Ldc_R4, i))
            | F64Const i -> il.Append(il.Create(OpCodes.Ldc_R8, i))

            | I32Add | I64Add | F32Add | F64Add -> il.Append(il.Create(OpCodes.Add))
            | I32Mul | I64Mul | F32Mul | F64Mul -> il.Append(il.Create(OpCodes.Mul))
            | I32Sub | I64Sub | F32Sub | F64Sub -> il.Append(il.Create(OpCodes.Sub))
            | I32DivS | I64DivS | F32Div | F64Div -> il.Append(il.Create(OpCodes.Div))
            | I32DivU | I64DivU -> il.Append(il.Create(OpCodes.Div_Un))

            | F64Abs ->
                let ext = ctx.md.ImportReference(typeof<System.Math>.GetMethod("Abs", [| typeof<double> |] ))
                il.Append(il.Create(OpCodes.Call, ext))
            | F64Sqrt ->
                let ext = ctx.md.ImportReference(typeof<System.Math>.GetMethod("Sqrt", [| typeof<double> |] ))
                il.Append(il.Create(OpCodes.Call, ext))
            | F64Ceil ->
                let ext = ctx.md.ImportReference(typeof<System.Math>.GetMethod("Ceiling", [| typeof<double> |] ))
                il.Append(il.Create(OpCodes.Call, ext))
            | F64Floor ->
                let ext = ctx.md.ImportReference(typeof<System.Math>.GetMethod("Floor", [| typeof<double> |] ))
                il.Append(il.Create(OpCodes.Call, ext))
            | F64Trunc ->
                let ext = ctx.md.ImportReference(typeof<System.Math>.GetMethod("Truncate", [| typeof<double> |] ))
                il.Append(il.Create(OpCodes.Call, ext))
            | F64Nearest ->
                let ext = ctx.md.ImportReference(typeof<System.Math>.GetMethod("Round", [| typeof<double> |] ))
                il.Append(il.Create(OpCodes.Call, ext))
            | F64Min ->
                let ext = ctx.md.ImportReference(typeof<System.Math>.GetMethod("Min", [| typeof<double>; typeof<double> |] ))
                il.Append(il.Create(OpCodes.Call, ext))
            | F64Max ->
                let ext = ctx.md.ImportReference(typeof<System.Math>.GetMethod("Max", [| typeof<double>; typeof<double> |] ))
                il.Append(il.Create(OpCodes.Call, ext))

            | F32Abs ->
                let ext = ctx.md.ImportReference(typeof<System.Math>.GetMethod("Abs", [| typeof<float32> |] ))
                il.Append(il.Create(OpCodes.Call, ext))
            | F32Sqrt ->
                let ext = ctx.md.ImportReference(typeof<System.Math>.GetMethod("Sqrt", [| typeof<float32> |] ))
                il.Append(il.Create(OpCodes.Call, ext))
            | F32Ceil ->
                let ext = ctx.md.ImportReference(typeof<System.Math>.GetMethod("Ceiling", [| typeof<float32> |] ))
                il.Append(il.Create(OpCodes.Call, ext))
            | F32Floor ->
                let ext = ctx.md.ImportReference(typeof<System.Math>.GetMethod("Floor", [| typeof<float32> |] ))
                il.Append(il.Create(OpCodes.Call, ext))
            | F32Trunc ->
                let ext = ctx.md.ImportReference(typeof<System.Math>.GetMethod("Truncate", [| typeof<float32> |] ))
                il.Append(il.Create(OpCodes.Call, ext))
            | F32Nearest ->
                let ext = ctx.md.ImportReference(typeof<System.Math>.GetMethod("Round", [| typeof<float32> |] ))
                il.Append(il.Create(OpCodes.Call, ext))
            | F32Min ->
                let ext = ctx.md.ImportReference(typeof<System.Math>.GetMethod("Min", [| typeof<float32>; typeof<float32> |] ))
                il.Append(il.Create(OpCodes.Call, ext))
            | F32Max ->
                let ext = ctx.md.ImportReference(typeof<System.Math>.GetMethod("Max", [| typeof<float32>; typeof<float32> |] ))
                il.Append(il.Create(OpCodes.Call, ext))

            | I32Eqz ->
                il.Append(il.Create(OpCodes.Ldc_I4_0))
                il.Append(il.Create(OpCodes.Ceq))
                
            | I64Eqz ->
                il.Append(il.Create(OpCodes.Ldc_I8, 0L))
                il.Append(il.Create(OpCodes.Ceq))
                
            | I32LtS | I64LtS | F32Lt | F64Lt -> il.Append(il.Create(OpCodes.Clt))
            | I32LtU | I64LtU -> il.Append(il.Create(OpCodes.Clt_Un))

            | I32GtS | I64GtS | F32Gt | F64Gt -> il.Append(il.Create(OpCodes.Cgt))
            | I32GtU | I64GtU -> il.Append(il.Create(OpCodes.Cgt_Un))

            | F32Neg -> il.Append(il.Create(OpCodes.Neg))
            | F64Neg -> il.Append(il.Create(OpCodes.Neg))

            | I32Eq | I64Eq | F32Eq | F64Eq -> il.Append(il.Create(OpCodes.Ceq))

            | I32And | I64And -> il.Append(il.Create(OpCodes.And))
            | I32Or | I64Or -> il.Append(il.Create(OpCodes.Or))
            | I32Xor | I64Xor -> il.Append(il.Create(OpCodes.Xor))

            | I32Shl | I64Shl -> il.Append(il.Create(OpCodes.Shl))
            | I32ShrS | I64ShrS -> il.Append(il.Create(OpCodes.Shr))
            | I32ShrU | I64ShrU -> il.Append(il.Create(OpCodes.Shr_Un))
            | I32RemS | I64RemS -> il.Append(il.Create(OpCodes.Rem))
            | I32RemU | I64RemU -> il.Append(il.Create(OpCodes.Rem_Un))

            | F32ConvertI32S | F32ConvertI64S | F32DemoteF64 -> il.Append(il.Create(OpCodes.Conv_R4))
            | F64ConvertI32S | F64ConvertI64S | F64PromoteF32 -> il.Append(il.Create(OpCodes.Conv_R8))

            | F32ConvertI32U | F32ConvertI64U ->
                il.Append(il.Create(OpCodes.Conv_R_Un))
                il.Append(il.Create(OpCodes.Conv_R4))

            | F64ConvertI32U | F64ConvertI64U ->
                il.Append(il.Create(OpCodes.Conv_R_Un))
                il.Append(il.Create(OpCodes.Conv_R8))

            | I32WrapI64 -> il.Append(il.Create(OpCodes.Conv_I4))
            | I64ExtendI32S | I64ExtendI32U -> il.Append(il.Create(OpCodes.Conv_I8))

            | I32TruncF32S | I32TruncF64S -> il.Append(il.Create(OpCodes.Conv_Ovf_I4))
            | I64TruncF32S | I64TruncF64S -> il.Append(il.Create(OpCodes.Conv_Ovf_I8))
            | I32TruncF32U | I32TruncF64U -> il.Append(il.Create(OpCodes.Conv_Ovf_I4_Un))
            | I64TruncF32U | I64TruncF64U -> il.Append(il.Create(OpCodes.Conv_Ovf_I8_Un))

            | I32Ne | I64Ne | F32Ne | F64Ne -> 
                il.Append(il.Create(OpCodes.Ceq))
                il.Append(il.Create(OpCodes.Ldc_I4_0))
                il.Append(il.Create(OpCodes.Ceq))

            | I32LeS | I64LeS ->
                il.Append(il.Create(OpCodes.Cgt))
                il.Append(il.Create(OpCodes.Ldc_I4_0))
                il.Append(il.Create(OpCodes.Ceq))

            | I32GeS | I64GeS ->
                il.Append(il.Create(OpCodes.Clt))
                il.Append(il.Create(OpCodes.Ldc_I4_0))
                il.Append(il.Create(OpCodes.Ceq))

            | I32LeU | I64LeU ->
                il.Append(il.Create(OpCodes.Cgt_Un))
                il.Append(il.Create(OpCodes.Ldc_I4_0))
                il.Append(il.Create(OpCodes.Ceq))

            | I32GeU | I64GeU ->
                il.Append(il.Create(OpCodes.Clt_Un))
                il.Append(il.Create(OpCodes.Ldc_I4_0))
                il.Append(il.Create(OpCodes.Ceq))

            | F32Le | F64Le ->
                il.Append(il.Create(OpCodes.Cgt_Un))
                il.Append(il.Create(OpCodes.Ldc_I4_0))
                il.Append(il.Create(OpCodes.Ceq))

            | F32Ge | F64Ge ->
                il.Append(il.Create(OpCodes.Clt_Un))
                il.Append(il.Create(OpCodes.Ldc_I4_0))
                il.Append(il.Create(OpCodes.Ceq))

            | Drop -> il.Append(il.Create(OpCodes.Pop))

            | Unreachable -> todo op
            | Select -> todo op
            | MemorySize _ -> todo op
            | MemoryGrow _ -> todo op
            | I32Clz -> todo op
            | I32Ctz -> todo op
            | I32Popcnt -> todo op
            | I32Rotl -> todo op
            | I32Rotr -> todo op
            | I64Clz -> todo op
            | I64Ctz -> todo op
            | I64Popcnt -> todo op
            | I64Rotl -> todo op
            | I64Rotr -> todo op
            | F32Copysign -> todo op
            | F64Copysign -> todo op
            | I32ReinterpretF32 -> todo op
            | I64ReinterpretF64 -> todo op
            | F32ReinterpretI32 -> todo op
            | F64ReinterpretI64 -> todo op

    let create_global gi idx bt =
        let name = 
            match gi.ig_name with
            | Some s -> s
            | None -> sprintf "global_%d" idx

        let typ = cecil_valtype bt gi.item.globaltype.typ

        let access = if gi.exported then FieldAttributes.Public else FieldAttributes.Private

        let method = 
            new FieldDefinition(
                name,
                access ||| Mono.Cecil.FieldAttributes.Static, 
                typ
                )

        method

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

        let access = if fi.exported then MethodAttributes.Public else MethodAttributes.Private

        let method = 
            new MethodDefinition(
                name,
                access ||| Mono.Cecil.MethodAttributes.Static, 
                return_type
                )

        method

    let gen_function_code ctx (mi : MethodRefInternal) =
        let a_locals =
            let a = System.Collections.Generic.List<ParamOrVar>()
            let get_name () =
                sprintf "p%d" (a.Count)
            for x in mi.func.if_typ.parms do
                let typ = cecil_valtype ctx.bt x
                let name = get_name()
                let def = new ParameterDefinition(name, ParameterAttributes.None, typ)
                a.Add(PV_Param { P_def = def; P_typ = x; })
            for loc in mi.func.code.locals do
                // TODO assert count > 0 ?
                for x = 1 to (int loc.count) do
                    let typ = cecil_valtype ctx.bt loc.localtype
                    let def = new VariableDefinition(typ)
                    a.Add(PV_Var { L_def = def; L_typ = loc.localtype })
            Array.ofSeq a
            
        mi.method.Body.InitLocals <- true
        for pair in a_locals do
            match pair with
            | PV_Param { P_def = def } -> mi.method.Parameters.Add(def)
            | PV_Var { L_def = def } -> mi.method.Body.Variables.Add(def)

        let tmps = prep_tmps ctx.bt mi.method

        let il = mi.method.Body.GetILProcessor()
        cecil_expr il ctx tmps a_locals mi.func.code.expr
        il.Append(il.Create(OpCodes.Ret))

    let create_methods ndx bt =
        let count_imports = count_function_imports ndx
        let prep_func i fi =
            match fi with
            | F_Imported s ->
                // TODO
                M_Imported { MethodRefImported.func = s }
            | F_Internal q ->
                let method = create_method q (count_imports + i) bt
                M_Internal { func = q; method = method; }

        let a_methods = Array.mapi prep_func ndx.FuncLookup
        a_methods

    let gen_code_for_methods ctx =
        for m in ctx.a_methods do
            match m with
            | M_Internal mi -> gen_function_code ctx mi
            | M_Imported _ -> ()

    let create_globals ndx bt =
        let count_imports = count_global_imports ndx

        let prep i gi =
            match gi with
            | G_Imported s ->
                // TODO
                GS_Imported { GlobalRefImported.glob = s }
            | G_Internal q ->
                let field = create_global q (count_imports + i) bt
                GS_Internal { glob = q; field = field; }

        let a_globals = Array.mapi prep ndx.GlobalLookup

        a_globals

    let gen_table_lookup ndx bt =
        let method = 
            new MethodDefinition(
                "__tbl_lookup",
                MethodAttributes.Private ||| MethodAttributes.Static,  // TODO SpecialName?  RTSpecialName?
                bt.typ_intptr // TODO is this right?
                )
        let il = method.Body.GetILProcessor()
        il.Append(il.Create(OpCodes.Nop))
        // TODO lots of stuff needed here
        method

    let gen_cctor ctx a_datas =
        let mem = ctx.mem
        let a_globals = ctx.a_globals
        let bt = ctx.bt
        let method = 
            new MethodDefinition(
                ".cctor",
                MethodAttributes.Private ||| MethodAttributes.Static,  // TODO SpecialName?  RTSpecialName?
                bt.typ_void
                )
        let il = method.Body.GetILProcessor()

        il.Append(il.Create(OpCodes.Ldc_I4, 64 * 1024))
        // TODO where is this freed?
        let ext = mem.Module.ImportReference(typeof<System.Runtime.InteropServices.Marshal>.GetMethod("AllocHGlobal", [| typeof<int32> |] ))
        il.Append(il.Create(OpCodes.Call, ext))
        il.Append(il.Create(OpCodes.Stfld, mem))

        let tmps = prep_tmps bt method

        for d in a_datas do
            // TODO emit code to load the resource and copy it to memory
            ()

        for g in a_globals do
            match g with
            | GS_Internal gi -> 
                cecil_expr il ctx tmps Array.empty gi.glob.item.init
                il.Append(il.Create(OpCodes.Stfld, gi.field))
            | GS_Imported _ -> ()

        method

    type DataStuff = {
        item : DataItem
        name : string
        resource : EmbeddedResource
        }

    let create_data_resources ctx sd =
        let f i d =
            let name = sprintf "data_%d" i
            let flags = ManifestResourceAttributes.Private
            let r = EmbeddedResource(name, flags, d.init)
            { item = d; name = name; resource = r; }
        Array.mapi f sd.datas

    let gen_assembly m assembly_name ns classname (ver : System.Version) (dest : System.IO.Stream) =
        let assembly = 
            AssemblyDefinition.CreateAssembly(
                new AssemblyNameDefinition(
                    assembly_name, 
                    ver
                    ), 
                assembly_name, 
                ModuleKind.Dll
                )

        let main_module = assembly.MainModule

        let bt = 
            {
                typ_i32 = main_module.TypeSystem.Int32
                typ_i64 = main_module.TypeSystem.Int64
                typ_f32 = main_module.TypeSystem.Single
                typ_f64 = main_module.TypeSystem.Double
                typ_void = main_module.TypeSystem.Void
                typ_intptr = main_module.TypeSystem.IntPtr
            }

        let container = 
            new TypeDefinition(
                ns,
                classname,
                TypeAttributes.Class ||| TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed, 
                main_module.TypeSystem.Object
                )

        main_module.Types.Add(container);

        let mem =
            new FieldDefinition(
                "__mem",
                FieldAttributes.Private ||| FieldAttributes.Static, 
                main_module.TypeSystem.IntPtr
                )
        container.Fields.Add(mem)

        let ndx = get_module_index m

        let a_globals = create_globals ndx bt

        for m in a_globals do
            match m with
            | GS_Internal mi -> container.Fields.Add(mi.field)
            | GS_Imported _ -> ()

        let a_methods = create_methods ndx bt

        for m in a_methods do
            match m with
            | M_Internal mi -> container.Methods.Add(mi.method)
            | M_Imported _ -> ()

        let tbl_lookup = gen_table_lookup ndx bt
        container.Methods.Add(tbl_lookup)

        let ctx =
            {
                md = container.Module
                bt = bt
                a_globals = a_globals
                a_methods = a_methods
                mem = mem
                tbl_lookup = tbl_lookup
            }

        gen_code_for_methods ctx

        let a_datas =
            match ndx.Data with
            | Some sd -> create_data_resources ctx sd
            | None -> Array.empty
        for d in a_datas do
            main_module.Resources.Add(d.resource)

        let cctor = gen_cctor ctx a_datas
        container.Methods.Add(cctor)

        assembly.Write(dest);

