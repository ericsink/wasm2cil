
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
        typ: ValType
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
        valtype : ValType
        }

    type CodeItem = {
        locals: Local list
        expr: Instruction list
        len : uint32 // TODO not sure we should save this
        }

    type CustomSection = {
        name : string
        data : byte[]
        }

    type ImportSection = {
        imports : ImportItem list
        }

    type TypeSection = {
        types : FuncType list
        }

    type FunctionSection = {
        funcs : uint32 list
        }

    type TableSection = {
        tables : TableType list
        }

    type MemorySection = {
        mems : MemType list
        }

    type GlobalSection = {
        globals : GlobalItem list
        }

    type ExportSection = {
        exports : ExportItem list
        }

    type ElementSection = {
        elems : ElementItem list
        }

    type CodeSection = {
        codes : CodeItem list
        }

    type DataSection = {
        datas : DataItem list
        }

    // TODO the labels below might want Section appended?
    type Section =
        | Custom of CustomSection
        | Type of TypeSection
        | Import of ImportSection
        | Function of FunctionSection
        | Table of TableSection
        | Memory of MemorySection
        | Global of GlobalSection
        | Export of ExportSection
        | Start of uint32
        | Element of ElementSection
        | Code of CodeSection
        | Data of DataSection

    type Module = {
        version: uint32
        sections: Section list
        }

