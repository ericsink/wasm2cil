
module wasm.args
    type MemArg = {
        align : uint32
        offset : uint32
        }

    type CallIndirectArg = {
        x: uint32
        other: byte
        }

    type BrTableArg = {
        v: uint32 list
        other: uint32
        }

