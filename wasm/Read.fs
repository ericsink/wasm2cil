
module wasm.read

    open wasm.def_instr
    open wasm.def_basic
    open wasm.def
    open wasm.read_basic
    open wasm.read_instr

    let read_name br =
        let len_name = read_var_u32 br
        let ba_name = read_bytes br len_name
        let name = System.Text.Encoding.UTF8.GetString(ba_name, 0, ba_name.Length);
        name
    
    let read_nameassoc br =
        let idx = read_var_u32 br
        let name = read_name br
        {
            NameAssoc.idx = idx
            name = name
        }

    let read_module_name_subsection br =
        let name = read_name br
        NameSubSection.Module name

    let read_namemap br =
        let count = read_var_u32 br
        let a = read_vector br count read_nameassoc
        a

    let read_indirectnameassoc br =
        let idx = read_var_u32 br
        let a = read_namemap br
        {
            IndirectNameAssoc.idx = idx
            map = a
        }

    let read_indirectnamemap br =
        let count = read_var_u32 br
        let a = read_vector br count read_indirectnameassoc
        a

    let read_function_names_subsection br =
        let a = read_namemap br
        NameSubSection.Function a

    let read_local_names_subsection br =
        let a = read_indirectnamemap br
        NameSubSection.Local a

    let read_name_subsection br =
        let id = read_byte br
        let offset = get_read_offset br
        let br_subsection = 
            let len = read_var_u32 br
            let ba_subsection = read_bytes br len
            BinaryWasmStream(ba_subsection)
        //printfn "read name subsection %d at %d with len %d" id offset (br_subsection.Length())
        match id with
        | 0uy -> read_module_name_subsection br_subsection
        | 1uy -> read_function_names_subsection br_subsection
        | 2uy -> read_local_names_subsection br_subsection
        | _ -> failwith "unknown name subsection id"

    let read_name_section ba =
        let br = BinaryWasmStream(ba)
        let sections = System.Collections.Generic.List<NameSubSection>()
        while get_remaining br > 0 do
            let s = read_name_subsection br
            sections.Add(s)
        Array.ofSeq sections

    let read_custom_section br =
        let name = read_name br
        let len = get_remaining br
        let ba = read_bytes br (len |> uint32)
        if name = "name" then
            Custom (Name { subsections = read_name_section ba })
        else
            Custom (Unknown { name = name; data = ba; })

    let read_valtype br =
        read_byte br |> make_valtype

    let read_functype br =
        let b = read_byte br // 0x60
        let count1 = read_var_u32 br
        let vec1 = read_vector br count1 read_valtype
        let count2 = read_var_u32 br
        let vec2 = read_vector br count2 read_valtype
        { parms = vec1; result = vec2 }

    let read_type_section br =
        let count = read_var_u32 br
        let a = read_vector br count read_functype
        Type { types = a }

    let read_limits br =
        match read_byte br with
        | 0x00uy ->
            let min = read_var_u32 br
            Min min
        | 0x01uy ->
            let min = read_var_u32 br
            let max = read_var_u32 br
            MinMax (min, max)
        | _ ->
            failwith "invalid first byte for limits"

    let read_idx br =
        read_var_u32 br

    let read_tabletype br =
        let elemtype = 
            match read_byte br with
            | 0x70uy -> FuncRef
            | _ -> failwith "unknown"
        let limits = read_limits br
        { elemtype = elemtype; limits = limits }

    let read_memtype br =
        let limits = read_limits br
        { limits = limits }

    let read_globaltype br =
        let globaltype = read_valtype br
        let mut = 
            match read_byte br with
            | 0x00uy -> false
            | 0x01uy -> true
            | _ -> failwith "mut bool"
        { typ = globaltype; mut = mut; }

    let read_importdesc br =
        let import_type = read_byte br
        match import_type with
        | 0x00uy -> read_idx br |> TypeIdx |> ImportFunc
        | 0x01uy -> read_tabletype br |> ImportTable
        | 0x02uy -> read_memtype br |> ImportMem
        | 0x03uy -> read_globaltype br |> ImportGlobal
        | _ -> failwith "invalid importdesc byte"
        
    let read_exportdesc br =
        match read_byte br with
        | 0x00uy -> read_idx br |> FuncIdx |> ExportFunc
        | 0x01uy -> read_idx br |> TableIdx |> ExportTable
        | 0x02uy -> read_idx br |> MemIdx |> ExportMem
        | 0x03uy -> read_idx br |> GlobalIdx |> ExportGlobal
        | _ -> failwith "invalid exportdesc byte"
        
    let read_import br =
        let m = read_name br
        let name = read_name br
        let desc = read_importdesc br
        { m = m; ImportItem.name = name; desc = desc }

    let read_export br =
        let name = read_name br
        let desc = read_exportdesc br
        { ExportItem.name = name; desc = desc }

    let read_expr br =
        let rec g depth a =
            let it = read_instruction br
            let b = it :: a
            let depth =
                match it with
                | Block _ -> depth + 1
                | Loop _ -> depth + 1
                | If _ -> depth + 1
                | End -> depth - 1
                | _ -> depth
            if depth = 0 then
                b
            else
                g depth b
        g 1 [] |> List.rev |> Array.ofList

    let read_global br =
        let gt = read_globaltype br
        let e = read_expr br
        { globaltype = gt; init = e }

    let read_data br =
        let memidx = read_idx br |> MemIdx
        let e = read_expr br
        let count = read_var_u32 br
        let a = read_bytes br count
        { memidx = memidx; offset = e; init = a }

    let read_elem br =
        let tableidx = read_idx br |> TableIdx
        let e = read_expr br
        let count = read_var_u32 br
        let f r =
            let i = read_idx r
            i |> FuncIdx
        let a = read_vector br count f
        { tableidx = tableidx; offset = e; init = a }

    let read_local br =
        let n = read_var_u32 br
        let valtype = read_valtype br
        { count = n; localtype = valtype }

    let read_code br =
        let len = read_var_u32 br
        let br = 
            let ba = read_bytes br len
            BinaryWasmStream(ba)
        let count = read_var_u32 br
        let locals = read_vector br count read_local
        let e = read_expr br
        { locals = locals; expr = e }

    let read_code_section br =
        let count = read_var_u32 br
        let a = read_vector br count read_code
        Code { codes = a }

    let read_import_section br =
        let count = read_var_u32 br
        let a = read_vector br count read_import
        Import { imports = a }

    let read_function_section br =
        let count = read_var_u32 br
        let f r =
            let i = read_idx r
            i |> TypeIdx
        let a = read_vector br count f
        Function { funcs = a }

    let read_table_section br =
        let count = read_var_u32 br
        let a = read_vector br count read_tabletype
        Table { tables = a }

    let read_memory_section br =
        let count = read_var_u32 br
        let a = read_vector br count read_memtype
        Memory { mems = a }

    let read_export_section br =
        let count = read_var_u32 br
        let a = read_vector br count read_export
        Export { exports = a }

    let read_start_section br =
        let idx = read_idx br |> FuncIdx
        Start idx

    let read_data_section br =
        let count = read_var_u32 br
        let a = read_vector br count read_data
        Data { datas = a }

    let read_global_section br =
        let count = read_var_u32 br
        let a = read_vector br count read_global
        Global { globals = a }

    let read_element_section br =
        let count = read_var_u32 br
        let a = read_vector br count read_elem
        Element { elems = a }

    let read_section br =
        let id = read_byte br
        let offset = get_read_offset br
        let br_section = 
            let len = read_var_u32 br
            let ba_section = read_bytes br len
            BinaryWasmStream(ba_section)
        //printfn "read section %d at %d with len %d" id offset (br_section.Length())
        match id with
        | 0uy -> read_custom_section br_section
        | 1uy -> read_type_section br_section
        | 2uy -> read_import_section br_section
        | 3uy -> read_function_section br_section
        | 4uy -> read_table_section br_section
        | 5uy -> read_memory_section br_section
        | 6uy -> read_global_section br_section
        | 7uy -> read_export_section br_section
        | 8uy -> read_start_section br_section
        | 9uy -> read_element_section br_section
        | 10uy -> read_code_section br_section
        | 11uy -> read_data_section br_section
        | _ -> failwith "unknown section id"

    let read_module br =
        let magic = read_u32 br
        let ver = read_u32 br

        let a =
            let sections = System.Collections.Generic.List<Section>()
            while get_remaining br > 0 do
                let s = read_section br
                sections.Add(s)
            Array.ofSeq sections
        
        {
            version = ver;
            sections = a;
        }

