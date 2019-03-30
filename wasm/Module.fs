
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
        n : uint32
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

    type ModuleIndex = {
        Type : TypeSection option
        Import : ImportSection option
        Function : FunctionSection option
        Table : TableSection option
        Memory : MemorySection option
        Global : GlobalSection option
        Export : ExportSection option
        Start : FuncIdx option
        Element : ElementSection option
        Code : CodeSection option
        Data : DataSection option
        // TODO list of custom sections?
        }

    let get_module_index m =
        {
            Type = Array.tryPick (fun x -> match x with | Type i -> Some i | _ -> None) m.sections
            Import = Array.tryPick (fun x -> match x with | Import i -> Some i | _ -> None) m.sections
            Function = Array.tryPick (fun x -> match x with | Function i -> Some i | _ -> None) m.sections
            Table = Array.tryPick (fun x -> match x with | Table i -> Some i | _ -> None) m.sections
            Memory = Array.tryPick (fun x -> match x with | Memory i -> Some i | _ -> None) m.sections
            Global = Array.tryPick (fun x -> match x with | Global i -> Some i | _ -> None) m.sections
            Export = Array.tryPick (fun x -> match x with | Export i -> Some i | _ -> None) m.sections
            Start = Array.tryPick (fun x -> match x with | Start i -> Some i | _ -> None) m.sections
            Element = Array.tryPick (fun x -> match x with | Element i -> Some i | _ -> None) m.sections
            Code = Array.tryPick (fun x -> match x with | Code i -> Some i | _ -> None) m.sections
            Data = Array.tryPick (fun x -> match x with | Data i -> Some i | _ -> None) m.sections
        }


