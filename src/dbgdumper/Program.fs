// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Mono.Cecil
open Mono.Cecil.Rocks
open Mono.Cecil.Cil
open Newtonsoft.Json


let private seqOfMonoCol (col: Mono.Collections.Generic.Collection<'t>) =
    Seq.unfold (fun i ->
        if i < col.Count then
            Some(col.[i], i + 1)
        else
            None)
        0

let private dumpMethod (meth: MethodDefinition) =
    printfn ">> Method: %A" meth
    let sequencePoints =
        seqOfMonoCol meth.DebugInformation.SequencePoints
        |> Seq.map (fun x -> (x.Offset, x))
        |> Map.ofSeq
    seqOfMonoCol meth.Body.Instructions
    |> Seq.indexed
    |> Seq.iter (fun (i, il) ->
        match Map.tryFind i sequencePoints with
        | Some(point) ->
            printfn "  %A (%d:%d %d:%d)" point.Document.Url point.StartLine point.StartColumn point.EndLine point.EndColumn
        | None -> ()
        printfn "  %A" il)
    ()

let private dumpType (ty: TypeDefinition) =
    printfn "> Ty: %A" ty
    seqOfMonoCol ty.Methods
    |> Seq.iter dumpMethod
    ()

let private dumpAssm (path: string)  =
    let opts = ReaderParameters(ReadingMode.Immediate)
    opts.ReadSymbols <- true
    let assm = AssemblyDefinition.ReadAssembly(path, opts)
    printfn "Runtime: %A" assm.MainModule.Runtime
    printfn "Runtime Version: %A" assm.MainModule.RuntimeVersion
    seqOfMonoCol assm.MainModule.Types
    |> Seq.iter dumpType

[<EntryPoint>]
let main argv =
    Seq.iter dumpAssm argv
    0 // return an integer exit code