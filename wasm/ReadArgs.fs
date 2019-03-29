
module wasm.read_args
    open wasm.args
    open wasm.read_basic

    let read_memarg (br: BinaryWasmStream) =
        let a = br.ReadVarUInt32()
        let o = br.ReadVarUInt32()
        { align = a; offset = o; }

    let read_callindirect (br: BinaryWasmStream) =
        let x = br.ReadVarUInt32()
        let o = br.ReadByte()
        { x = x; other = o; }

    let read_brtable (br: BinaryWasmStream) =
        let read_item (br: BinaryWasmStream) =
            br.ReadVarUInt32()
        let count = br.ReadVarUInt32() |> int
        let a = read_vector br count read_item
        let o = br.ReadVarUInt32()
        { v = a; other = o; }

