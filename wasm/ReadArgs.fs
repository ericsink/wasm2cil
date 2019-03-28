
module wasm.readargs
    open wasm.args
    open wasm.buffer

    let read_vector (br: BinaryWasmStream) count f =
        if count = 0 then 
            []
        else
            let rec g a =
                let it = f br
                let b = it :: a
                if b.Length = count then b else g b
            g [] |> List.rev

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

