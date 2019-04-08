
module wasm.write_args
    open wasm.write_basic
    open wasm.def_basic

    let write_brtable w t =
        write_var_u32 w (uint32 t.v.Length)
        for (LabelIdx it) in t.v do
            write_var_u32 w it
        let (LabelIdx other) = t.other
        write_var_u32 w other

    let write_memarg w t =
        write_var_u32 w t.align
        write_var_u32 w t.offset

    let write_callindirect w t =
        let (TypeIdx x) = t.typeidx
        write_var_u32 w x
        write_byte w t.other

