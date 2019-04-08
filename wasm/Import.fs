
module wasm.import

    open System
    open System.Reflection

    open Mono.Cecil

    open wasm.module_index

    let import_function (md : ModuleDefinition) (s : ImportedFunc) (a : System.Reflection.Assembly) =
        let typ = a.GetType(s.m)
        let method = typ.GetMethod(s.name) // TODO should include params in the lookup here
        let mref = md.ImportReference(method)
        mref

