
open System

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
                    let assembly = System.Reflection.Assembly.GetAssembly(typeof<wasi_unstable>)
                    let ba = 
                        use ms = new System.IO.MemoryStream()
                        let id = "test"
                        let ns = id
                        let classname = "foo"
                        let ver = new System.Version(1, 0, 0, 0)
                        let settings = {
                            memory = MemorySetting.AlwaysImportPairFrom "wasi_unstable"
                            profile = if option_profile.ParsedValue then ProfileSetting.Yes assembly else ProfileSetting.No
                            trace = if option_trace.ParsedValue then TraceSetting.Yes assembly else TraceSetting.No
                            env = Some assembly
                            }
                        gen_assembly settings m id ns classname ver ms
                        ms.ToArray()
                    System.IO.File.WriteAllBytes(dest.Value, ba)
                    )
                )
            )
        ) |> ignore
    app.OnExecute(
        (fun () ->
            System.Console.Error.WriteLine("specify subcommand")
            )
        )

    app.Execute(argv)

