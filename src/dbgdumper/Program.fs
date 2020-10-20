// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Mono.Cecil
open Mono.Cecil.Rocks
open Mono.Cecil.Cil

let private dumpInfo (path: string) =
    let opts = ReaderParameters(ReadingMode.Immediate)
    opts.ReadSymbols <- true
    let assm = AssemblyDefinition.ReadAssembly(path, opts)
    printfn "Body: %A" assm.MainModule.EntryPoint.Body.Instructions
    printfn "Debug Info Scopes: %A" assm.MainModule.EntryPoint.DebugInformation.Scope
    printfn "Debug Info Seq Points: %A" assm.MainModule.EntryPoint.DebugInformation.SequencePoints
    ()

[<EntryPoint>]
let main argv =
    Seq.iter dumpInfo argv
    0 // return an integer exit code