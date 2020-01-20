
module wasm.cil

    open wasm.def_basic
    open Mono.Cecil
    open Mono.Cecil.Cil

    // TODO need void too.  or maybe void is: type option.
    // it only happens for methods, right?

    type VariableType =
        | SystemType of System.Type // TODO weird
        | ValType of ValType

    type GenTypes = {
        ns : string
        md : ModuleDefinition
        }

    let valtype_to_ceciltype typs vt =
        // TODO should these be signed or unsigned?
        match vt with
        | I32 -> typs.md.TypeSystem.Int32
        | I64 -> typs.md.TypeSystem.Int64
        | F32 -> typs.md.TypeSystem.Single
        | F64 -> typs.md.TypeSystem.Double

    let make_struct_name st =
        (sprintf "%A" st)
            .Replace(" ", "")
            .Replace("\n", "")
            .Replace("\r", "")
            .Replace("\t", "")
            .Replace("[", "")
            .Replace("]", "")
            .Replace("|", "")
            .Replace("{", "")
            .Replace("}", "")
            .Replace(";", "")

    let general_type_to_cecil_type typs vt =
        match vt with
        | SystemType t -> typs.md.ImportReference(t)
        | ValType t -> valtype_to_ceciltype typs t

    type Label = {
        id : int32
        }

    type Variable = {
        id : int32
        typ : VariableType
        }

    type MyInstruction =
        | Add
        | And
        | Br of Label
        | Brfalse of Label
        | Brtrue of Label
        | Box of ValType
        | Call of MethodReference
        | Calli of CallSite
        | Callvirt of MethodReference
        | Ceq
        | Cgt
        | Cgt_Un
        | Clt
        | Clt_Un
        | Conv_I
        | Conv_Ovf_I4
        | Conv_Ovf_I8
        | Conv_Ovf_I4_Un
        | Conv_Ovf_I8_Un
        | Conv_I1
        | Conv_I2
        | Conv_I4
        | Conv_I8
        | Conv_U4
        | Conv_U8
        | Conv_R_Un
        | Conv_R4
        | Conv_R8
        | Cpblk
        | Div
        | Div_Un
        | Dup
        | Initblk
        | Label of Label // not actually an instruction
        | Ldarg of ParameterDefinition
        | Ldarga of ParameterDefinition
        | Ldc_I4 of int32
        | Ldc_I4_M1
        | Ldc_I4_0
        | Ldc_I4_1
        | Ldc_I4_2
        | Ldc_I4_3
        | Ldc_I4_4
        | Ldc_I4_5
        | Ldc_I4_6
        | Ldc_I4_7
        | Ldc_I4_8
        | Ldc_I8 of int64
        | Ldc_R4 of float32
        | Ldc_R8 of double
        | Ldftn of MethodReference
        | Ldind_I
        | Ldind_I1
        | Ldind_I2
        | Ldind_I4
        | Ldind_I8
        | Ldind_R4
        | Ldind_R8
        | Ldind_U1
        | Ldind_U2
        | Ldind_U4
        | Ldloc of Variable
        | Ldloca of Variable
        | Ldnull
        | Ldsfld of FieldReference
        | Ldsflda of FieldReference
        | Ldstr of string
        | Localloc
        | Mul
        | Neg
        | Newarr of System.Type
        | Newobj of MethodReference
        | Nop
        | Or
        | Pop
        | Rem
        | Rem_Un
        | Shl
        | Shr
        | Shr_Un
        | Stelem_Ref
        | Stind_I
        | Stind_I1
        | Stind_I2
        | Stind_I4
        | Stind_I8
        | Stind_R4
        | Stind_R8
        | Starg of ParameterDefinition
        | Stloc of Variable
        | Stsfld of FieldReference
        | Switch of Label[]
        | Sub
        | Ret
        | Throw
        | Xor

    let make_cil_instruction (il: ILProcessor) (d_variables : System.Collections.Generic.Dictionary<Variable,VariableDefinition>) (d_labels : System.Collections.Generic.Dictionary<Label,Mono.Cecil.Cil.Instruction>) typs instr =
        match instr with
        | Add -> il.Create(OpCodes.Add)
        | And -> il.Create(OpCodes.And)
        | Br x -> il.Create(OpCodes.Br, d_labels.[x])
        | Brfalse x -> il.Create(OpCodes.Brfalse, d_labels.[x])
        | Brtrue x -> il.Create(OpCodes.Brtrue, d_labels.[x])
        | Box x -> il.Create(OpCodes.Box, valtype_to_ceciltype typs x)
        | Call x -> il.Create(OpCodes.Call, x)
        | Calli x -> il.Create(OpCodes.Calli, x)
        | Callvirt x -> il.Create(OpCodes.Callvirt, x)
        | Ceq -> il.Create(OpCodes.Ceq)
        | Cgt -> il.Create(OpCodes.Cgt)
        | Cgt_Un -> il.Create(OpCodes.Cgt_Un)
        | Clt -> il.Create(OpCodes.Clt)
        | Clt_Un -> il.Create(OpCodes.Clt_Un)
        | Conv_I -> il.Create(OpCodes.Conv_I)
        | Conv_I1 -> il.Create(OpCodes.Conv_I1)
        | Conv_I2 -> il.Create(OpCodes.Conv_I2)
        | Conv_I4 -> il.Create(OpCodes.Conv_I4)
        | Conv_I8 -> il.Create(OpCodes.Conv_I8)
        | Conv_R_Un -> il.Create(OpCodes.Conv_R_Un)
        | Conv_R4 -> il.Create(OpCodes.Conv_R4)
        | Conv_R8 -> il.Create(OpCodes.Conv_R8)
        | Conv_U4 -> il.Create(OpCodes.Conv_U4)
        | Conv_U8 -> il.Create(OpCodes.Conv_U8)
        | Conv_Ovf_I4 -> il.Create(OpCodes.Conv_Ovf_I4)
        | Conv_Ovf_I8 -> il.Create(OpCodes.Conv_Ovf_I8)
        | Conv_Ovf_I4_Un -> il.Create(OpCodes.Conv_Ovf_I4_Un)
        | Conv_Ovf_I8_Un -> il.Create(OpCodes.Conv_Ovf_I8_Un)
        | Cpblk -> il.Create(OpCodes.Cpblk)
        | Div -> il.Create(OpCodes.Div)
        | Div_Un -> il.Create(OpCodes.Div_Un)
        | Dup -> il.Create(OpCodes.Dup)
        | Initblk -> il.Create(OpCodes.Initblk)
        | Label x -> d_labels.[x]
        | Ldarg x -> il.Create(OpCodes.Ldarg, x)
        | Ldarga x -> il.Create(OpCodes.Ldarga, x)
        | Ldc_I4 x -> il.Create(OpCodes.Ldc_I4, x)
        | Ldc_I4_M1 -> il.Create(OpCodes.Ldc_I4_M1)
        | Ldc_I4_0 -> il.Create(OpCodes.Ldc_I4_0)
        | Ldc_I4_1 -> il.Create(OpCodes.Ldc_I4_1)
        | Ldc_I4_2 -> il.Create(OpCodes.Ldc_I4_2)
        | Ldc_I4_3 -> il.Create(OpCodes.Ldc_I4_3)
        | Ldc_I4_4 -> il.Create(OpCodes.Ldc_I4_4)
        | Ldc_I4_5 -> il.Create(OpCodes.Ldc_I4_5)
        | Ldc_I4_6 -> il.Create(OpCodes.Ldc_I4_6)
        | Ldc_I4_7 -> il.Create(OpCodes.Ldc_I4_7)
        | Ldc_I4_8 -> il.Create(OpCodes.Ldc_I4_8)
        | Ldc_I8 x -> il.Create(OpCodes.Ldc_I8, x)
        | Ldc_R4 x -> il.Create(OpCodes.Ldc_R4, x)
        | Ldc_R8 x -> il.Create(OpCodes.Ldc_R8, x)
        | Ldftn x -> il.Create(OpCodes.Ldftn, x)
        | Ldind_I -> il.Create(OpCodes.Ldind_I)
        | Ldind_I1 -> il.Create(OpCodes.Ldind_I1)
        | Ldind_I2 -> il.Create(OpCodes.Ldind_I2)
        | Ldind_I4 -> il.Create(OpCodes.Ldind_I4)
        | Ldind_I8 -> il.Create(OpCodes.Ldind_I8)
        | Ldind_R4 -> il.Create(OpCodes.Ldind_R4)
        | Ldind_R8 -> il.Create(OpCodes.Ldind_R8)
        | Ldind_U1 -> il.Create(OpCodes.Ldind_U1)
        | Ldind_U2 -> il.Create(OpCodes.Ldind_U2)
        | Ldind_U4 -> il.Create(OpCodes.Ldind_U4)
        | Ldloc x -> il.Create(OpCodes.Ldloc, d_variables.[x])
        | Ldloca x -> il.Create(OpCodes.Ldloca, d_variables.[x])
        | Ldnull -> il.Create(OpCodes.Ldnull)
        | Ldsfld x -> il.Create(OpCodes.Ldsfld, x)
        | Ldsflda x -> il.Create(OpCodes.Ldsflda, x)
        | Ldstr x -> il.Create(OpCodes.Ldstr, x)
        | Localloc -> il.Create(OpCodes.Localloc)
        | Mul -> il.Create(OpCodes.Mul)
        | Neg -> il.Create(OpCodes.Neg)
        | Newarr x -> il.Create(OpCodes.Newarr, typs.md.ImportReference(x))
        | Newobj x -> il.Create(OpCodes.Newobj, x)
        | Nop -> il.Create(OpCodes.Nop)
        | Or -> il.Create(OpCodes.Or)
        | Pop -> il.Create(OpCodes.Pop)
        | Rem -> il.Create(OpCodes.Rem)
        | Rem_Un -> il.Create(OpCodes.Rem_Un)
        | Shl -> il.Create(OpCodes.Shl)
        | Shr -> il.Create(OpCodes.Shr)
        | Shr_Un -> il.Create(OpCodes.Shr_Un)
        | Stelem_Ref -> il.Create(OpCodes.Stelem_Ref)
        | Stind_I -> il.Create(OpCodes.Stind_I)
        | Stind_I1 -> il.Create(OpCodes.Stind_I1)
        | Stind_I2 -> il.Create(OpCodes.Stind_I2)
        | Stind_I4 -> il.Create(OpCodes.Stind_I4)
        | Stind_I8 -> il.Create(OpCodes.Stind_I8)
        | Stind_R4 -> il.Create(OpCodes.Stind_R4)
        | Stind_R8 -> il.Create(OpCodes.Stind_R8)
        | Starg x -> il.Create(OpCodes.Starg, x)
        | Stloc x -> il.Create(OpCodes.Stloc, d_variables.[x])
        | Stsfld x -> il.Create(OpCodes.Stsfld, x)
        | Switch x -> il.Create(OpCodes.Switch, Array.map (fun lab -> d_labels.[lab]) x)
        | Sub -> il.Create(OpCodes.Sub)
        | Ret -> il.Create(OpCodes.Ret)
        | Throw -> il.Create(OpCodes.Throw)
        | Xor -> il.Create(OpCodes.Xor)

    type CilWriter() =
        let a = System.Collections.Generic.List<MyInstruction>()
        let labels = System.Collections.Generic.List<Label>()
        let variables = System.Collections.Generic.List<Variable>()
        let stk = System.Collections.Generic.Stack<int>()
        let mutable cannot = false

        let find_label_uses lab =
            let b = System.Collections.Generic.List<int>()
            for i = 0 to a.Count - 1 do
                let instr = a.[i]
                match instr with
                | Br q
                | Brfalse q
                | Brtrue q ->
                    if q = lab then b.Add(i)
                | Switch a ->
                    match Array.tryFind (fun x -> x = lab) a with
                    | Some _ -> b.Add(i)
                    | None -> ()
                | _ -> ()
            b

        let find_variable_uses v =
            let b = System.Collections.Generic.List<int>()
            for i = 0 to a.Count - 1 do
                let instr = a.[i]
                match instr with
                | Ldloc q
                | Ldloca q
                | Stloc q ->
                    if q = v then b.Add(i)
                | _ -> ()
            b

        let find_first_label () =
            let b = System.Collections.Generic.List<int>()
            for i = 0 to a.Count - 1 do
                let instr = a.[i]
                match instr with
                | Label q -> b.Add(i)
                | _ -> ()
            if b.Count > 0 then
                b.[0]
            else
                System.Int32.MaxValue

        let find_label lab =
            let b = System.Collections.Generic.List<int>()
            for i = 0 to a.Count - 1 do
                let instr = a.[i]
                match instr with
                | Label q ->
                    if q = lab then b.Add(i)
                | _ -> ()
            if b.Count = 1 then
                b.[0]
            else
                failwith "find_label"

        let rm_branch_to_next_instruction () =
            for i = 0 to a.Count - 1 do
                let instr = a.[i]
                match instr with
                | Br lab ->
                    let j = find_label lab
                    if i + 1 = j then
                        a.[i] <- Nop
                | _ -> ()

        let rm_unused_labels () =
            let rm = System.Collections.Generic.List<Label>()
            for lab in labels do
                let uses = find_label_uses lab
                if uses.Count = 0 then
                    let i = find_label lab
                    a.[i] <- Nop
                    rm.Add(lab)
            for lab in rm do
                labels.Remove(lab) |> ignore

        let rm_trivial_variables () =
            let rm = System.Collections.Generic.List<Variable>()
            for v in variables do
                let uses = find_variable_uses v
                if (uses.Count = 2) && (uses.[1] = uses.[0] + 1)  then
                    let check_store =
                        match a.[uses.[0]] with
                        | Stloc _ -> true
                        | _ -> false
                    let check_load =
                        match a.[uses.[1]] with
                        | Ldloc _ -> true
                        | _ -> false
                    if check_store && check_load then
                        a.[uses.[0]] <- Nop
                        a.[uses.[1]] <- Nop
                        rm.Add(v)
            for v in rm do
                variables.Remove(v) |> ignore

        let rm_dead_variables () =
            let first_label = find_first_label ()
            let rm = System.Collections.Generic.List<Variable>()
            for v in variables do
                let v1_uses = find_variable_uses v
                if (v1_uses.Count > 0) then
                    let v1_last_use = v1_uses.[v1_uses.Count - 1]
                    let check_load =
                        match a.[v1_last_use] with
                        | Ldloc _ -> true
                        | _ -> false
                    if check_load && (v1_last_use < first_label) then
                        match a.[v1_last_use + 1] with
                        | Stloc v2 ->
                            let v2_uses = find_variable_uses v2
                            if v2_uses.[0] = v1_last_use + 1 then
                                // v2 can be replaced with v1
                                //printfn "removing a dead var"
                                for i in v2_uses do
                                    match a.[i] with
                                    | Ldloc _ ->
                                        a.[i] <- Ldloc v
                                    | Ldloca _ ->
                                        a.[i] <- Ldloca v
                                    | Stloc _ ->
                                        a.[i] <- Stloc v
                                a.[v1_last_use] <- Nop
                                a.[v1_last_use + 1] <- Nop
                                rm.Add(v2)
                        | _ -> ()
            for v in rm do
                variables.Remove(v) |> ignore

        let rm_undef_variables () =
            let rm = System.Collections.Generic.List<Variable>()
            for v in variables do
                let v1_uses = find_variable_uses v
                if (v1_uses.Count = 1) then
                    let v1_use = v1_uses.[0]
                    let check_load =
                        match a.[v1_use] with
                        | Ldloc _ -> true
                        | _ -> false
                    if check_load then
                        match a.[v1_use + 1] with
                        | Stloc v2 ->
                            // v1 is just an undef
                            //printfn "removing undef var"
                            a.[v1_use] <- Nop
                            a.[v1_use + 1] <- Nop
                            rm.Add(v)
                        | _ -> ()
            for v in rm do
                variables.Remove(v) |> ignore

        let pop () =
            stk.Pop() |> ignore

        let push () =
            stk.Push(0)

        let fix_stack instr =
            if cannot then
                ()
            else
                match instr with
                | Add
                | And
                | Ceq
                | Cgt
                | Cgt_Un
                | Clt
                | Clt_Un
                | Div
                | Div_Un
                | Mul
                | Or
                | Rem
                | Rem_Un
                | Shl
                | Shr
                | Shr_Un
                | Sub
                | Xor ->
                    pop ()
                    pop ()
                    push ()
                | Box x ->
                    pop ()
                    push ()
                | Br x
                | Brfalse x
                | Brtrue x ->
                    cannot <- true
                | Conv_I
                | Conv_Ovf_I4
                | Conv_Ovf_I8
                | Conv_Ovf_I4_Un
                | Conv_Ovf_I8_Un
                | Conv_I1
                | Conv_I2
                | Conv_I4
                | Conv_I8
                | Conv_R_Un
                | Conv_R4
                | Conv_R8
                | Conv_U4
                | Conv_U8 ->
                    pop ()
                    push ()
                | Cpblk
                | Initblk ->
                    pop ()
                    pop ()
                    pop ()
                | Dup ->
                    pop ()
                    push ()
                    push ()
                | Label x ->
                    ()
                | Ldarg x ->
                    push ()
                | Ldarga x ->
                    push ()
                | Ldc_I4 x ->
                    push ()
                | Ldc_I4_M1 ->
                    push ()
                | Ldc_I4_0
                | Ldc_I4_1
                | Ldc_I4_2
                | Ldc_I4_3
                | Ldc_I4_4
                | Ldc_I4_5
                | Ldc_I4_6
                | Ldc_I4_7
                | Ldc_I4_8 ->
                    push ()
                | Ldc_I8 x ->
                    push ()
                | Ldc_R4 x ->
                    push ()
                | Ldc_R8 x ->
                    push ()
                | Ldftn x ->
                    push ()
                | Ldind_I
                | Ldind_I1
                | Ldind_I2
                | Ldind_I4
                | Ldind_I8
                | Ldind_R4
                | Ldind_R8
                | Ldind_U1
                | Ldind_U2
                | Ldind_U4 ->
                    pop ()
                    push ()
                | Ldloc x ->
                    push ()
                | Ldloca x ->
                    push ()
                | Ldnull ->
                    push ()
                | Ldsfld x ->
                    push ()
                | Ldsflda x ->
                    push ()
                | Ldstr x ->
                    push ()
                | Localloc ->
                    pop ()
                    push ()
                | Neg ->
                    pop ()
                    push ()
                | Newarr x ->
                    pop ()
                    push ()
                | Pop ->
                    pop ()
                | Stelem_Ref ->
                    pop ()
                    pop ()
                    pop ()
                | Stind_I
                | Stind_I1
                | Stind_I2
                | Stind_I4
                | Stind_I8
                | Stind_R4
                | Stind_R8 ->
                    pop ()
                    pop ()
                | Starg x ->
                    pop ()
                | Stloc x ->
                    pop ()
                | Stsfld x ->
                    pop ()
                | Switch x ->
                    pop ()
                | Nop
                | Ret ->
                    ()
                | Call x ->
                    // TODO distinguish vararg?
                    for i = 0 to x.Parameters.Count - 1 do
                        pop ()
                    if x.ReturnType.Name <> "Void" then
                        push ()
                | Calli x ->
                    // TODO distinguish vararg?
                    pop () // func ptr
                    for i = 0 to x.Parameters.Count - 1 do
                        pop ()
                    if x.ReturnType.Name <> "Void" then
                        push ()
                | Callvirt x ->
                    cannot <- true
                | Newobj x ->
                    for i = 0 to x.Parameters.Count - 1 do
                        pop ()
                    push ()
                | Throw ->
                    pop ()
                    cannot <- true

        member this.BeginInstruction() =
            stk.Clear()
            cannot <- false

        member this.EndInstruction(desc : string) =
            if cannot then
                //printfn "cannot: %s" desc
                ()
            else
                if stk.Count <> 0 then
                    printfn "STACK (%d items): %s" (stk.Count) desc
                () // TODO check stack

        member this.Append(instr : MyInstruction) =
            fix_stack instr
            a.Add(instr)

        member this.NewLabel() =
            let r = { id = labels.Count }
            labels.Add(r)
            r

        member this.NewVariable(tr : ValType) =
            let r = { 
                Variable.id = variables.Count 
                typ = ValType tr
                }
            variables.Add(r)
            r

        member this.NewVariable(tr : System.Type) =
            let r = { 
                Variable.id = variables.Count 
                typ = SystemType tr
                }
            variables.Add(r)
            r

        member this.Finish(method : MethodDefinition, typs : GenTypes) =
            rm_branch_to_next_instruction ()
            rm_unused_labels ()
            rm_trivial_variables ()
            rm_dead_variables ()
            rm_undef_variables ()
            let a =
                a 
                |> Seq.filter (fun x -> match x with | Nop -> false | _ -> true)
                |> Array.ofSeq

// TODO need map of types to cecil types so we can look up fields
            let il = method.Body.GetILProcessor()
            let d_labels = System.Collections.Generic.Dictionary<Label,Mono.Cecil.Cil.Instruction>()
            for lab in labels do
                d_labels.Add(lab, il.Create(OpCodes.Nop))
            let d_variables = System.Collections.Generic.Dictionary<Variable,VariableDefinition>()
            for v in variables do
                let ctyp = general_type_to_cecil_type typs v.typ
                let vdef = new VariableDefinition(ctyp)
                method.Body.Variables.Add(vdef)
                d_variables.Add(v, vdef)
            for instr in a do
                let i2 = make_cil_instruction il d_variables d_labels typs instr
                il.Append(i2)

