
module wasm.errors

    type WrongOperandType(s : string) =
        inherit System.Exception(s)

    type OperandStackUnderflow(s : string) =
        inherit System.Exception(s)

    type ExtraBlockResult(s : string) =
        inherit System.Exception(s)

