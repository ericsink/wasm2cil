
module wasm.def_basic
    type ValType =
        | I32
        | I64
        | F32
        | F64

    let make_valtype b =
        match b with
        | 0x7Fuy -> I32
        | 0x7Euy -> I64
        | 0x7Duy -> F32
        | 0x7Cuy -> F64
        | _ -> failwith "unknown valtype"
    
    let encode_valtype vt =
        match vt with
        | I32 -> 0x7Fuy
        | I64 -> 0x7Euy
        | F32 -> 0x7Duy
        | F64 -> 0x7Cuy

    type TypeIdx = TypeIdx of uint32
    type FuncIdx = FuncIdx of uint32
    type TableIdx = TableIdx of uint32
    type MemIdx = MemIdx of uint32
    type GlobalIdx = GlobalIdx of uint32
    type LocalIdx = LocalIdx of uint32
    type LabelIdx = LabelIdx of uint32

    type MemArg = {
        align : uint32
        offset : uint32
        }

    type CallIndirectArg = {
        x: uint32 // TODO what is this? typeidx ?
        other: byte
        }

    // TODO LabelIdx below
    type BrTableArg = {
        v: uint32[]
        other: uint32
        }

