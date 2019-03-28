
module wasm.m

    open wasm.buffer
    open wasm.parse
    open wasm.instr
    open wasm.args

    type Limits =
        | Min of uint32
        | MinMax of uint32 * uint32

    type ValType =
        | I32
        | I64
        | F32
        | F64

    type FuncType = {
        parms: ValType list
        result: ValType list
        }

    type ElemType =
        | FuncRef

    type TableType = {
        elemtype: ElemType
        limits: Limits
        }

    type MemType = {
        limits: Limits
        }

    type GlobalType = {
        globaltype: ValType
        mut: bool
        }

    type ImportDesc =
        | TypeIdx of uint32
        | TableType of TableType
        | MemType of MemType
        | GlobalType of GlobalType

    type ExportDesc =
        | FuncIdx of uint32
        | TableIdx of uint32
        | MemIdx of uint32
        | GlobalIdx of uint32

    type ExportItem = {
        name : string
        desc : ExportDesc
        }

    type ImportItem = {
        m : string
        name : string
        desc : ImportDesc
        }

    type GlobalItem = {
        globaltype: GlobalType
        expr: Instruction list
        }

    type ElementItem = {
        tableidx : uint32
        expr: Instruction list
        init: uint32 list
        }

    type DataItem = {
        memidx : uint32
        expr: Instruction list
        init: byte[]
        }

    type Local = {
        n : uint32
        valtype : byte
        }

    type CodeItem = {
        locals: Local list
        expr: Instruction list
        len : int
        }

    type Section =
        | Custom of string
        | Type of FuncType list
        | Import of ImportItem list
        | Function of uint32 list
        | Table of TableType list
        | Memory of MemType list
        | Global of GlobalItem list
        | Export of ExportItem list
        | Start of uint32
        | Element of ElementItem list
        | Code of CodeItem list
        | Data of DataItem list

    type Module = {
        version: uint32
        sections: Section list
        }

    let read_name (br: BinaryWasmStream) =
        let len_name = br.ReadVarUInt32()
        let ba_name = br.ReadBytes(len_name)
        let name = System.Text.Encoding.UTF8.GetString(ba_name, 0, ba_name.Length);
        name

    let read_section_custom (br: BinaryWasmStream) =
        let name = read_name br
        Custom name

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

    let read_section_type (br: BinaryWasmStream) =
        let count = br.ReadVarUInt32() |> int
        let a = read_vector br count read_functype
        Type a

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
        { globaltype = globaltype; mut = mut; }

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
        let valtype = br.ReadByte()
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

    let read_section_code (br: BinaryWasmStream) =
        let count = br.ReadVarUInt32() |> int
        let a = read_vector br count read_code
        Code a

    let read_section_import (br: BinaryWasmStream) =
        let count = br.ReadVarUInt32() |> int
        let a = read_vector br count read_import
        Import a

    let read_section_function (br: BinaryWasmStream) =
        let count = br.ReadVarUInt32() |> int
        let a = read_vector br count read_idx
        Function a

    let read_section_table (br: BinaryWasmStream) =
        let count = br.ReadVarUInt32() |> int
        let a = read_vector br count read_tabletype
        Table a

    let read_section_memory (br: BinaryWasmStream) =
        let count = br.ReadVarUInt32() |> int
        let a = read_vector br count read_memtype
        Memory a

    let read_section_export (br: BinaryWasmStream) =
        let count = br.ReadVarUInt32() |> int
        let a = read_vector br count read_export
        Export a

    let read_section_start (br: BinaryWasmStream) =
        let idx = read_idx br
        Start idx

    let read_section_data (br: BinaryWasmStream) =
        let count = br.ReadVarUInt32() |> int
        let a = read_vector br count read_data
        Data a

    let read_section_global (br: BinaryWasmStream) =
        let count = br.ReadVarUInt32() |> int
        let a = read_vector br count read_global
        Global a

    let read_section_element (br: BinaryWasmStream) =
        let count = br.ReadVarUInt32() |> int
        let a = read_vector br count read_elem
        Element a

    let read_section (br: BinaryWasmStream) =
        let id = br.ReadByte()
        let br_section = 
            let len = br.ReadVarUInt32()
            let ba_section = br.ReadBytes(len)
            BinaryWasmStream(ba_section)
        match id with
        | 0uy -> read_section_custom br_section
        | 1uy -> read_section_type br_section
        | 2uy -> read_section_import br_section
        | 3uy -> read_section_function br_section
        | 4uy -> read_section_table br_section
        | 5uy -> read_section_memory br_section
        | 6uy -> read_section_global br_section
        | 7uy -> read_section_export br_section
        | 8uy -> read_section_start br_section
        | 9uy -> read_section_element br_section
        | 10uy -> read_section_code br_section
        | 11uy -> read_section_data br_section
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

