
module wasm.write_args
    open wasm.write_basic
    open wasm.def_basic

    let write_brtable w t =
        write_var_u32 w (uint32 t.v.Length)
        for it in t.v do
            write_var_u32 w it
        write_var_u32 w t.other

    let write_memarg w t =
        write_var_u32 w t.align
        write_var_u32 w t.offset

    let write_callindirect w t =
        write_var_u32 w t.x
        write_byte w t.other

