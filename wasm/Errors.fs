
module wasm.errors

    type WrongOperandType(s : string) =
        inherit System.Exception(s)

