
module wasm.builder

    open wasm.def_instr
    open wasm.def_basic
    open wasm.def

    type FunctionBuilder () =
        let instructions = System.Collections.Generic.List<Instruction>()
        let parms = System.Collections.Generic.List<ValType>()
        let locals = System.Collections.Generic.List<ValType>()

        member val Name : string option = None with get,set
        member val ReturnType : ValType option = None with get,set

        member this.AddParam (t : ValType) =
            parms.Add(t)

        member this.AddLocal (t : ValType) =
            locals.Add(t)

        member this.Add (i : Instruction) =
            instructions.Add(i)

        member this.Params
            with get() = parms

        member this.Locals
            with get() = locals

        member this.Instructions
            with get() = instructions

        member this.FuncType() =
            {
                parms = Array.ofSeq parms
                result =
                    match this.ReturnType with
                    | Some x -> [| x |]
                    | None ->  [|  |]
            }

    type ModuleBuilder () =
        let fbuilders = System.Collections.Generic.List<FunctionBuilder>()
        let elems = System.Collections.Generic.List<int * uint32>()
        let imports = System.Collections.Generic.List<ImportItem>()

        member this.AddFunction(fb : FunctionBuilder) =
            fbuilders.Add(fb)

        member this.AddElement(offset : int, fidx : uint32) =
            elems.Add((offset, fidx))

        member this.AddImport(it : ImportItem) =
            imports.Add(it)

        member this.CreateModule() =
            let types = System.Collections.Generic.List<FuncType>()
            let funcs = System.Collections.Generic.List<TypeIdx>()
            let exports = System.Collections.Generic.List<ExportItem>()
            let codes = System.Collections.Generic.List<CodeItem>()

            let find ft =
                let a = Array.ofSeq types
                Array.tryFindIndex (fun t -> t = ft) a

            for i = 0 to (fbuilders.Count - 1) do
                let fb = fbuilders.[i]
                let ft = fb.FuncType()
                let typeidx = 
                    match find ft with
                    | Some i -> i
                    | None ->
                        let i = types.Count
                        types.Add(ft)
                        i
                funcs.Add(TypeIdx (uint32 typeidx))
                let fidx = FuncIdx (uint32 i)
                match fb.Name with
                | Some s -> exports.Add({ name = s; desc = ExportFunc fidx})
                | None -> ()
                let locals = 
                    fb.Locals
                    |> Array.ofSeq
                    |> Array.map (fun x -> { count = 1u; localtype = x; })
                codes.Add({ locals = locals; expr = Array.ofSeq fb.Instructions; })

            let sections = System.Collections.Generic.List<Section>()
            let s_type = { types = Array.ofSeq types }
            let s_function = { funcs = Array.ofSeq funcs }
            let s_export = { exports = Array.ofSeq exports }
            let s_code = { codes = Array.ofSeq codes }

            sections.Add(Type s_type)
            sections.Add(Function s_function)
            sections.Add(Export s_export)
            sections.Add(Code s_code)

            if elems.Count > 0 then
                let s_table = { tables = [| { elemtype = FuncRef; limits = Min (uint32 elems.Count); } |] }
                sections.Add(Table s_table)

                let a_elems =
                    elems
                    |> Array.ofSeq
                    |> Array.map (fun (off,fidx) -> { tableidx = TableIdx 0u; offset = [| I32Const off ; End; |]; init = [| FuncIdx fidx |]; })

                let s_elem = { elems = a_elems }
                sections.Add(Element s_elem)

            if imports.Count > 0 then
                let s_import = { imports = Array.ofSeq imports }
                sections.Add(Import s_import)
                
            {
                version = 1u
                sections = (Array.ofSeq sections)
            }


