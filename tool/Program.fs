
open System
open System.Linq

open System.ComponentModel.DataAnnotations
open McMaster.Extensions.CommandLineUtils
open McMaster.Extensions.CommandLineUtils.Validation

open wasm.read_basic
open wasm.read
open wasm.write
open wasm.cecil

[<EntryPoint>]
let main argv =
    let app = new CommandLineApplication()
    app.HelpOption() |> ignore
    app.Command("compile",
        (fun (cmd : CommandLineApplication) ->
            cmd.HelpOption() |> ignore
            let option_trace = cmd.Option<bool>("--trace", "Whether to insert tracing calls", CommandOptionType.NoValue)
            let option_profile = cmd.Option<bool>("--profile", "Whether to insert profiling calls", CommandOptionType.NoValue)

            // TODO want accepts existing file validator
            let src = cmd.Argument<string>("wasm_module", "The Wasm module to be compiled").IsRequired()

            let dest = cmd.Argument("dll", "Where to save the .NET assembly").IsRequired()

            cmd.OnExecute(
                (fun () ->
                    let br = BinaryWasmStream(System.IO.File.ReadAllBytes(src.Value))
                    let m = read_module br
                    let mem_assembly = System.Reflection.Assembly.GetAssembly(typeof<sg_wasm>)
                    let wasi_assembly = System.Reflection.Assembly.GetAssembly(typeof<wasi_unstable>)
                    let ba = 
                        use ms = new System.IO.MemoryStream()
                        let id = "test"
                        let ns = id
                        let classname = "foo"
                        let ver = new System.Version(1, 0, 0, 0)
                        let settings = {
                            memory = MemorySetting.AlwaysImportPairFrom "sg_wasm"
                            profile = if option_profile.ParsedValue then ProfileSetting.Yes wasi_assembly else ProfileSetting.No
                            trace = if option_trace.ParsedValue then TraceSetting.Yes wasi_assembly else TraceSetting.No
                            //trace = TraceSetting.Yes wasi_assembly
                            references = 
                                [| 
                                    wasi_assembly 
                                    mem_assembly
                                |]
                            }
                        gen_assembly settings m id ns classname ver ms
                        ms.ToArray()
                    System.IO.File.WriteAllBytes(dest.Value, ba)
                    )
                )
            )
        ) |> ignore
    app.Command("run",
        (fun (cmd : CommandLineApplication) ->
            cmd.HelpOption() |> ignore
            let option_trace = cmd.Option<bool>("--trace", "Whether to insert tracing calls", CommandOptionType.NoValue)
            let option_profile = cmd.Option<bool>("--profile <bool>", "Whether to insert profiling calls", CommandOptionType.SingleValue)

            // TODO want accepts existing file validator
            let src = cmd.Argument<string>("wasm_module", "The Wasm module to be compiled").IsRequired()

            cmd.AllowArgumentSeparator <- true
            cmd.OnExecute(
                (fun () ->
                    let br = BinaryWasmStream(System.IO.File.ReadAllBytes(src.Value))
                    let m = read_module br
                    let wasi_assembly = System.Reflection.Assembly.GetAssembly(typeof<wasi_unstable>)
                    let mem_assembly = System.Reflection.Assembly.GetAssembly(typeof<sg_wasm>)
                    let ba = 
                        use ms = new System.IO.MemoryStream()
                        let id = "test"
                        let ns = id
                        let classname = "foo"
                        let ver = new System.Version(1, 0, 0, 0)
                        let settings = {
                            memory = MemorySetting.AlwaysImportPairFrom "sg_wasm"
                            profile = if option_profile.ParsedValue then ProfileSetting.Yes wasi_assembly else ProfileSetting.No
                            trace = if option_trace.ParsedValue then TraceSetting.Yes wasi_assembly else TraceSetting.No
                            //trace = TraceSetting.Yes wasi_assembly
                            references = 
                                [| 
                                    wasi_assembly 
                                    mem_assembly
                                |]
                            }
                        gen_assembly settings m id ns classname ver ms
                        ms.ToArray()
                    let a = System.Reflection.Assembly.Load(ba)
                    let fullname = sprintf "%s.%s" "test" "foo"
                    let t = a.GetType(fullname)
                    if t = null then
                        failwith "type not found"
                    let mi = t.GetMethod("_start", [| |])
                    if mi = null then
                        failwith "entry point not found"

                    let newargs =
                        let z = [| "TODO" |]
                        z.Concat(cmd.RemainingArguments).ToArray()
                    wasi_unstable.set_args(newargs);

                    let mutable rc = 0
                    try
                        let ret = mi.Invoke(null, null)
                        if ret <> null then
                                rc <- unbox<int> ret
                    with
                    | :? System.Reflection.TargetInvocationException as e ->
                        let e = e.InnerException
                        match e with
                        | :?  ProcExitException as e -> rc <- e.ReturnCode
                        | _ -> raise e

                    if option_profile.ParsedValue then
                        __profile.Report(System.Console.Error)
                    )
                )
            )
        ) |> ignore
    app.Command("rundll",
        (fun (cmd : CommandLineApplication) ->
            cmd.HelpOption() |> ignore

            // TODO want accepts existing file validator
            let dll = cmd.Argument<string>("dll", "The DLL already compiled").IsRequired()

            cmd.AllowArgumentSeparator <- true
            cmd.OnExecute(
                (fun () ->
                    let ba = System.IO.File.ReadAllBytes(dll.Value)
                    let a = System.Reflection.Assembly.Load(ba)
                    let fullname = sprintf "%s.%s" "test" "foo"
                    let t = a.GetType(fullname)
                    if t = null then
                        failwith "type not found"
                    let mi = t.GetMethod("_start", [| |])
                    if mi = null then
                        failwith "entry point not found"

                    let newargs =
                        let z = [| "TODO" |]
                        z.Concat(cmd.RemainingArguments).ToArray()
                    wasi_unstable.set_args(newargs);

                    let mutable rc = 0
                    try
                        let ret = mi.Invoke(null, null)
                        if ret <> null then
                                rc <- unbox<int> ret
                    with
                    | :? System.Reflection.TargetInvocationException as e ->
                        let e = e.InnerException
                        match e with
                        | :?  ProcExitException as e -> rc <- e.ReturnCode
                        | _ -> raise e

                    )
                )
            )
        ) |> ignore
    app.OnExecute(
        (fun () ->
            app.ShowHelp()
            )
        )

    app.Execute(argv)

