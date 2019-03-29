
module wasm.read_args
    open wasm.args
    open wasm.read_basic

    let read_memarg br =
        let a = read_var_u32 br
        let o = read_var_u32 br
        { align = a; offset = o; }

    let read_callindirect br =
        let x = read_var_u32 br
        let o = read_byte br
        { x = x; other = o; }

    let read_brtable br =
        let read_item br =
            read_var_u32 br
        let count = read_var_u32 br
        let a = read_vector br count read_item
        let o = read_var_u32 br
        { v = a; other = o; }

