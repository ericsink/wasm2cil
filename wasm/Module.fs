
module wasm.def

    open wasm.def_basic
    open wasm.def_instr

    type Limits =
        | Min of uint32
        | MinMax of uint32 * uint32

    type FuncType = {
        parms: ValType[]
        result: ValType[]
        }

    let function_result_type ftype =
        if ftype.result.Length = 0 then
            None
        else if ftype.result.Length = 1 then
            ftype.result.[0] |> Some
        else
            failwith "not implemented"

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
        init: Instruction[]
        }

    type ElementItem = {
        tableidx : TableIdx
        offset: Instruction[]
        init: FuncIdx[]
        }

    type DataItem = {
        memidx : MemIdx
        offset: Instruction[]
        init: byte[]
        }

    type Local = {
        count : uint32
        localtype : ValType
        }

    type CodeItem = {
        locals: Local[]
        expr: Instruction[]
        }

    type CustomSection = {
        name : string
        data : byte[]
        }

    type ImportSection = {
        imports : ImportItem[]
        }

    type TypeSection = {
        types : FuncType[]
        }

    type FunctionSection = {
        funcs : TypeIdx[]
        }

    type TableSection = {
        tables : TableType[]
        }

    type MemorySection = {
        mems : MemType[]
        }

    type GlobalSection = {
        globals : GlobalItem[]
        }

    type ExportSection = {
        exports : ExportItem[]
        }

    type ElementSection = {
        elems : ElementItem[]
        }

    type CodeSection = {
        codes : CodeItem[]
        }

    type DataSection = {
        datas : DataItem[]
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
        sections: Section[]
        }

