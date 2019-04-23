
module fsgen.types

    type ValType =
    | I32
    | I64
    | F32
    | F64
    | V128

    type OpcodeInfo = {
        rtype: ValType option;
        type1: ValType option;
        type2: ValType option;
        type3: ValType option;
        mem_size: int;
        prefix: int option;
        code: int;
        name: string;
        text: string;
    }


