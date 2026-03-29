module Feersum.XPNew

open Feersum.CompilerServices.Text
open Feersum.CompilerServices.Syntax
open Feersum.CompilerServices.NewBindingTest
open System.IO
open Feersum.CompilerServices.Syntax.Tree
open Feersum.CompilerServices.Syntax.Parse
open Feersum.CompilerServices.Diagnostics

let runXpNewRepl args =
    let registry = SourceRegistry.empty ()

    let progs =
        List.map (fun path -> Parse.readProgram registry path (File.ReadAllText path)) args

    progs
    |> Seq.iter (fun prog ->
        if ParseResult.hasErrors prog then
            dumpDiagnostics prog.Diagnostics
        else
            Expand.expandProgram prog.Root |> printfn "%A")
