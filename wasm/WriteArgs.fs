
module wasm.write_args
    open wasm.write_basic
    open wasm.args

    let write_brtable w t =
        write_var_int w t.v.Length
        for it in t.v do
            write_var_uint32 w it
        write_var_uint32 w t.other

    let write_memarg w t =
        write_var_uint32 w t.align
        write_var_uint32 w t.offset

    let write_callindirect w t =
        write_var_uint32 w t.x
        write_byte w t.other

