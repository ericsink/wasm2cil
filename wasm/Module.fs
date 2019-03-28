
module wasm.def

    open wasm.instr

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

