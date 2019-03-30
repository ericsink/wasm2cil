
module wasm.stringify_args
    open wasm.def_basic

    type StringifyArgs = {
        stringify_funcidx : FuncIdx -> string
        stringify_localidx : LocalIdx -> string
        stringify_globalidx : GlobalIdx -> string
        stringify_labelidx : LabelIdx -> string
        stringify_brtable : BrTableArg -> string
        stringify_memarg : MemArg -> string
        stringify_callindirect : CallIndirectArg -> string
        }

