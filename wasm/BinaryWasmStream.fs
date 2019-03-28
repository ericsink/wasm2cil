
module wasm.buffer

    type BinaryWasmStream(ba: byte[]) =
        let buf = ba
        let mutable offset = 0

        let read_byte () =
            let b = buf.[offset]
            offset <- offset + 1
            b

        let rec read_var_signed count (value: int64) =
            let b = read_byte ()
            let result = value ||| (int64 (b &&& 0x7Fuy) <<< (count * 7))
            if (b &&& 0x80uy) = 0uy then
                let sign = -1L <<< ((count + 1) * 7)
                if ((sign >>> 1) &&& result) <> 0L then
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

        let read_bytes len =
            let last = offset + len - 1
            let ba = buf.[offset..last]
            offset <- offset + int len
            ba

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

            let v = (b3 <<< 24) ||| (b2 <<< 16) ||| (b1 <<< 8) ||| (b0 <<< 0)
            v

        member this.ReadVarUInt32() =
            read_var_unsigned 0 0u

        member this.ReadVarInt32() =
            read_var_signed 0 0L |> int32

        member this.ReadVarInt64() =
            read_var_signed 0 0L

        member this.ReadFloat32() =
            let ba = read_bytes 4
            0 |> float32 // TODO

        member this.ReadFloat64() =
            let ba = read_bytes 8
            0 |> double // TODO

        member this.ReadBytes(len: uint32) =
            read_bytes (int len)

