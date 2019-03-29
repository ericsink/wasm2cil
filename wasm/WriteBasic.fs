
module wasm.write_basic

    let write_byte (w : System.IO.BinaryWriter) (b : byte) =
        w.Write(b)

    let write_uint32 (w : System.IO.BinaryWriter) (v : uint32) =
        w.Write(v)

    let write_blob (w : System.IO.BinaryWriter) (ba : byte[]) =
        w.Write(ba)

    let write_f32 (w : System.IO.BinaryWriter) (f : float32) =
        w.Write(f)

    let write_f64 (w : System.IO.BinaryWriter) (f : double) =
        w.Write(f)

    let write_var_uint32 w (n : uint32) =
        let rec g v =
            let after = v >>> 7
            let b = 
                let a = byte (v &&& 0x7Fu)
                let b = if after = 0u then a else a ||| 0x80uy
                b
            write_byte w b
            if after <> 0u then
                g after
        g n

    let write_var_int w (n : int) =
        let n = uint32 n
        write_var_uint32 w n

    let write_var_i64 w (n : int64) =
        let rec g v =
            let after = v >>> 7
            let b = byte (v &&& 0x7FL)
            let finishing = ( (after = 0L) && ( (b &&& 0x40uy) = 0uy) ) || ( (after = -1L) && ( (b &&& 0x40uy) <> 0uy) )
            let b = if finishing then b else b ||| 0x80uy
            write_byte w b
            if not finishing then
                g after
        g n

    let write_var_i32 w (n : int32) =
        write_var_i64 w (int64 n)

