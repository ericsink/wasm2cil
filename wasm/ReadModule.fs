
module wasm.read

    open wasm.instr
    open wasm.def
    open wasm.buffer
    open wasm.read_instr

    let read_name (br: BinaryWasmStream) =
        let len_name = br.ReadVarUInt32()
        let ba_name = br.ReadBytes(len_name)
        let name = System.Text.Encoding.UTF8.GetString(ba_name, 0, ba_name.Length);
        name

    let read_custom_section (br: BinaryWasmStream) =
        let name = read_name br
        let len = br.Remaining()
        let ba = br.ReadBytes(len |> uint32)
        Custom { name = name; data = ba; }

    let read_valtype (br: BinaryWasmStream) =
        match br.ReadByte() with
        | 0x7Fuy -> I32
        | 0x7Euy -> I64
        | 0x7Duy -> F32
        | 0x7Cuy -> F64
        | _ -> failwith "unknown valtype"

    let read_functype (br: BinaryWasmStream) =
        let b = br.ReadByte() // 0x60
        let count1 = br.ReadVarUInt32() |> int
        let vec1 = read_vector br count1 read_valtype
        let count2 = br.ReadVarUInt32() |> int
        let vec2 = read_vector br count2 read_valtype
        { parms = vec1; result = vec2 }

    let read_type_section (br: BinaryWasmStream) =
        let count = br.ReadVarUInt32() |> int
        let a = read_vector br count read_functype
        Type { types = a }

    let read_limits (br: BinaryWasmStream) =
        match br.ReadByte() with
        | 0x00uy ->
            let min = br.ReadVarUInt32()
            Min min
        | 0x01uy ->
            let min = br.ReadVarUInt32()
            let max = br.ReadVarUInt32()
            MinMax (min, max)
        | _ ->
            failwith "invalid first byte for limits"

    let read_idx (br: BinaryWasmStream) =
        br.ReadVarUInt32()

    let read_tabletype (br: BinaryWasmStream) =
        let elemtype = 
            match br.ReadByte() with
            | 0x70uy -> FuncRef
            | _ -> failwith "unknown"
        let limits = read_limits br
        { elemtype = elemtype; limits = limits }

    let read_memtype (br: BinaryWasmStream) =
        let limits = read_limits br
        { limits = limits }

    let read_globaltype (br: BinaryWasmStream) =
        let globaltype = read_valtype br
        let mut = 
            match br.ReadByte() with
            | 0x00uy -> false
            | 0x01uy -> true
            | _ -> failwith "mut bool"
        { typ = globaltype; mut = mut; }

    let read_importdesc (br: BinaryWasmStream) =
        let import_type = br.ReadByte()
        match import_type with
        | 0x00uy -> read_idx br |> TypeIdx
        | 0x01uy -> read_tabletype br |> TableType
        | 0x02uy -> read_memtype br |> MemType
        | 0x03uy -> read_globaltype br |> GlobalType
        | _ -> failwith "invalid importdesc byte"
        
    let read_exportdesc (br: BinaryWasmStream) =
        match br.ReadByte() with
        | 0x00uy -> read_idx br |> FuncIdx
        | 0x01uy -> read_idx br |> TableIdx
        | 0x02uy -> read_idx br |> MemIdx
        | 0x03uy -> read_idx br |> GlobalIdx
        | _ -> failwith "invalid exportdesc byte"
        
    let read_import (br: BinaryWasmStream) =
        let m = read_name br
        let name = read_name br
        let desc = read_importdesc br
        { m = m; ImportItem.name = name; desc = desc }

    let read_export (br: BinaryWasmStream) =
        let name = read_name br
        let desc = read_exportdesc br
        { ExportItem.name = name; desc = desc }

    let read_expr (br: BinaryWasmStream) =
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
        g 1 [] |> List.rev

    let read_global (br: BinaryWasmStream) =
        let gt = read_globaltype br
        let e = read_expr br
        { globaltype = gt; expr = e }

    let read_byte (br: BinaryWasmStream) =
        br.ReadByte()

    let read_data (br: BinaryWasmStream) =
        let memidx = read_idx br
        let e = read_expr br
        let count = br.ReadVarUInt32()
        let a = br.ReadBytes(count)
        { memidx = memidx; expr = e; init = a }

    let read_elem (br: BinaryWasmStream) =
        let tableidx = read_idx br
        let e = read_expr br
        let count = br.ReadVarUInt32() |> int
        let a = read_vector br count read_idx
        { tableidx = tableidx; expr = e; init = a }

    let read_local (br: BinaryWasmStream) =
        let n = br.ReadVarUInt32()
        let valtype = read_valtype br
        { n = n; valtype = valtype }

    let read_code (br: BinaryWasmStream) =
        let br = 
            let len = br.ReadVarUInt32()
            let ba = br.ReadBytes(len)
            BinaryWasmStream(ba)
        let count = br.ReadVarUInt32() |> int
        let locals = read_vector br count read_local
        let e = read_expr br
        { CodeItem.len = br.Length(); locals = locals; expr = e }

    let read_code_section (br: BinaryWasmStream) =
        let count = br.ReadVarUInt32() |> int
        let a = read_vector br count read_code
        Code { codes = a }

    let read_import_section (br: BinaryWasmStream) =
        let count = br.ReadVarUInt32() |> int
        let a = read_vector br count read_import
        Import { imports = a }

    let read_function_section (br: BinaryWasmStream) =
        let count = br.ReadVarUInt32() |> int
        let a = read_vector br count read_idx
        Function { funcs = a }

    let read_table_section (br: BinaryWasmStream) =
        let count = br.ReadVarUInt32() |> int
        let a = read_vector br count read_tabletype
        Table { tables = a }

    let read_memory_section (br: BinaryWasmStream) =
        let count = br.ReadVarUInt32() |> int
        let a = read_vector br count read_memtype
        Memory { mems = a }

    let read_export_section (br: BinaryWasmStream) =
        let count = br.ReadVarUInt32() |> int
        let a = read_vector br count read_export
        Export { exports = a }

    let read_start_section (br: BinaryWasmStream) =
        let idx = read_idx br
        Start idx

    let read_data_section (br: BinaryWasmStream) =
        let count = br.ReadVarUInt32() |> int
        let a = read_vector br count read_data
        Data { datas = a }

    let read_global_section (br: BinaryWasmStream) =
        let count = br.ReadVarUInt32() |> int
        let a = read_vector br count read_global
        Global { globals = a }

    let read_element_section (br: BinaryWasmStream) =
        let count = br.ReadVarUInt32() |> int
        let a = read_vector br count read_elem
        Element { elems = a }

    let read_section (br: BinaryWasmStream) =
        let id = br.ReadByte()
        let br_section = 
            let len = br.ReadVarUInt32()
            let ba_section = br.ReadBytes(len)
            BinaryWasmStream(ba_section)
        printfn "read section %d with len %d" id (br_section.Length())
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

    let read_module (br: BinaryWasmStream) =
        let magic = br.ReadUInt32()
        let ver = br.ReadUInt32()

        let a =
            let sections = System.Collections.Generic.List<Section>()
            while br.Remaining() > 0 do
                let s = read_section br
                sections.Add(s)
            List.ofSeq sections
        
        {
            version = ver;
            sections = a;
        }

