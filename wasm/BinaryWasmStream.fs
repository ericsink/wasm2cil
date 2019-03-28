
module wasm.buffer

    type BinaryWasmStream(ba: byte[]) =
        let buf = ba
        let mutable offset = 0

        let read_byte () =
            let b = buf.[offset]
            offset <- offset + 1
            b

        let rec read_var_signed count (value: int32) =
            let b = read_byte ()
            let result = value ||| (int32 (b &&& 0x7Fuy) <<< (count * 7))
            if (b &&& 0x80uy) = 0uy then
                let sign = -1 <<< ((count + 1) * 7)
                if ((sign >>> 1) &&& result) <> 0 then
                    result ||| sign
                else
                    result
            else
                read_var_signed (count + 1) result

        let rec read_var_unsigned count (value: uint32) =
            let b = read_byte ()
            let result = value ||| (uint32 (b &&& 0x7Fuy) <<< (count * 7))
            if (b &&& 0x80uy) = 0uy then
                result
            else
                read_var_unsigned (count + 1) result

        member this.ReadByte() =
            let b = buf.[offset]
            offset <- offset + 1
            b

        member this.Remaining() =
            buf.Length - offset

        member this.Length() =
            buf.Length

        member this.ReadUInt32() =
            let b0 = this.ReadByte() |> uint32
            let b1 = this.ReadByte() |> uint32
            let b2 = this.ReadByte() |> uint32
            let b3 = this.ReadByte() |> uint32

            // TODO shifting here is probably wrong
            let v = (b0 <<< 24) ||| (b1 <<< 16) ||| (b2 <<< 8) ||| (b3 <<< 0)
            v

        member this.ReadVarUInt32() =
            read_var_unsigned 0 0u

        member this.ReadVarInt32() =
            read_var_signed 0 0

        member this.ReadBytes(len: uint32) =
            let last = offset + int len - 1
            let ba = buf.[offset..last]
            offset <- offset + int len
            ba

