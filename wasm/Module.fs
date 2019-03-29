
module wasm.def

    open wasm.def_basic
    open wasm.def_instr

    type Limits =
        | Min of uint32
        | MinMax of uint32 * uint32

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
        | ImportFunc of TypeIdx
        | ImportTable of TableType
        | ImportMem of MemType
        | ImportGlobal of GlobalType

    type ExportDesc =
        | ExportFunc of FuncIdx
        | ExportTable of TableIdx
        | ExportMem of MemIdx
        | ExportGlobal of GlobalIdx

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
        init: Instruction list
        }

    type ElementItem = {
        tableidx : TableIdx
        offset: Instruction list
        init: FuncIdx list
        }

    type DataItem = {
        memidx : MemIdx
        offset: Instruction list
        init: byte[]
        }

    type Local = {
        n : uint32
        localtype : ValType
        }

    type CodeItem = {
        locals: Local list
        expr: Instruction list
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
        funcs : TypeIdx list
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
        | Start of FuncIdx
        | Element of ElementSection
        | Code of CodeSection
        | Data of DataSection

    type Module = {
        version: uint32
        sections: Section list
        }

