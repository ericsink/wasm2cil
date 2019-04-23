
open System
open System.IO
open System.Text.RegularExpressions
open System.Linq

[<EntryPoint>]
let main argv =
    printfn "public static partial class wasi_unstable"
    printfn "{"
    let lines = File.ReadAllLines(argv.[0])
    for s in lines do
        let r = """#define +(?<name>[_A-Za-z0-9]+) +\((?<t>[_A-Z0-9]+)\((?<v>[^)]*)\)\)"""
        let regx = Regex(r)
        let a = regx.Matches(s);
        if (a <> null) && (a.Count = 1) then
            let m = a.First()
            let name = m.Groups.["name"].Value.Trim()
            let t = m.Groups.["t"].Value.Trim()
            let v = m.Groups.["v"].Value.Trim()
            let typ =
                match t with
                | "UINT8_C" -> "byte"
                | "UINT16_C" -> "ushort"
                | "UINT32_C" -> "uint"
                | "UINT64_C" -> "ulong"
                | _ -> failwith (sprintf "unknown type: %s" t)
            printfn "    const %s %s = %s;" typ name v
    printfn "}"
    0 // return an integer exit code

