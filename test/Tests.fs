module Tests

open System
open Xunit
open wasm.def
open wasm.read
open wasm.write
open wasm.cecil

let newid () =
    Guid
        .NewGuid()
        .ToString()
        .Replace("{", "")
        .Replace("}", "")
        .Replace("-", "")

[<Fact>]
let ``empty module`` () =
    let m = {
        version = 1u
        sections = Array.empty
        }

    let id = newid ()
    let ba = 
        use ms = new System.IO.MemoryStream()
        gen_assembly m id ms
        ms.ToArray()
    System.Reflection.Assembly.Load(ba)

    Assert.True(true)

