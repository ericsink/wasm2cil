
module wasm.read_basic

    type BinaryWasmStream(ba: byte[]) =
        let buf = ba
        let mutable offset = 0

        member this.ReadByte() =
            let b = buf.[offset]
            offset <- offset + 1
            b

        member this.ReadBytes(len: uint32) =
            let last = offset + (int len) - 1
            let ba = buf.[offset..last]
            offset <- offset + int len
            ba

        member this.Offset() =
            offset

        member this.Length() =
            buf.Length

    let read_byte (br: BinaryWasmStream) =
        br.ReadByte()

    let read_bytes (br: BinaryWasmStream) (len :uint32) =
        br.ReadBytes(len)

    let get_read_offset (br: BinaryWasmStream) =
        br.Offset()

    let get_remaining (br: BinaryWasmStream) =
        br.Length() - br.Offset()

    let read_var_i32 br =
        let mutable result = 0
        let mutable shift = 0
        let mutable loop = true
        let mutable b = 0uy
        while loop do
            b <- read_byte br
            result <- result ||| ((int (b &&& 0x7fuy)) <<< shift)
            shift <- shift + 7
            loop <- (b &&& 0x80uy) <> 0uy
        if (shift < 32) && ((b &&& 0x40uy) <> 0uy) then
            result <- result ||| ((~~~0) <<< shift)
        result

// TODO can these two functions share code?

    let read_var_i64 br =
        let mutable result = 0L
        let mutable shift = 0
        let mutable loop = true
        let mutable b = 0uy
        while loop do
            b <- read_byte br
            result <- result ||| ((int64 (b &&& 0x7fuy)) <<< shift)
            shift <- shift + 7
            loop <- (b &&& 0x80uy) <> 0uy
        if (shift < 64) && ((b &&& 0x40uy) <> 0uy) then
            result <- result ||| ((~~~0L) <<< shift)
        result

    // TODO chg this to follow the wikipedia pseudocode?
    let rec private read_var_unsigned br count (value: uint64) =
        let b = read_byte br
        let result = value ||| (uint64 (b &&& 0x7Fuy) <<< (count * 7))
        if (b &&& 0x80uy) = 0uy then
            result
        else
            read_var_unsigned br (count + 1) result

    let read_var_u32 br =
        read_var_unsigned br 0 0uL |> uint32

    let read_f32 br =
        let ba = read_bytes br 4u
        // TODO BitConverter
        use ms = new System.IO.MemoryStream(ba)
        use br = new System.IO.BinaryReader(ms)
        br.ReadSingle()

    let read_f64 br =
        let ba = read_bytes br 8u
        // TODO BitConverter
        use ms = new System.IO.MemoryStream(ba)
        use br = new System.IO.BinaryReader(ms)
        br.ReadDouble()

    let read_u32 br =
        let b0 = read_byte br |> uint32
        let b1 = read_byte br |> uint32
        let b2 = read_byte br |> uint32
        let b3 = read_byte br |> uint32

        let v = (b3 <<< 24) ||| (b2 <<< 16) ||| (b1 <<< 8) ||| (b0 <<< 0)
        v

    let read_vector (br: BinaryWasmStream) count f =
        if count = 0u then 
            Array.empty
        else
            let rec g a =
                let it = f br
                let b = it :: a
                if (uint32 b.Length) = count then b else g b
            g [] |> List.rev |> Array.ofList

