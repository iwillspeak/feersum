module Feersum.Repl

open System

open Feersum.Version
open Feersum.CompilerServices.Syntax
open Feersum.CompilerServices.Eval
open Feersum.CompilerServices.Diagnostics
open Feersum.Core

/// Read a single line of user input and parse it into a
/// syntax tree. If the input can't be parsed then read
/// again.
let rec private read () : AstNode =
    let line = ReadLine.Read("§> ")

    match Parse.readExpr line with
    | (node, []) -> node
    | (_, diagnostics) ->
        diagnostics |> dumpDiagnostics
        read ()

/// Print an object out to the console. Used to serialise the external
/// representation form an eval
let private print value =
    value |> cilExternalRepr |> printfn "}= %s"

/// Read, Execute, Print Loop
///
/// Repeatedly reads input and prints output
let rec private repl evaluator =
    try
        match (read >> evaluator) () with
        | Result.Ok _ -> ()
        | Result.Error diags -> dumpDiagnostics (diags)
    with
    | ex -> eprintfn "Exception: %A" ex

    repl evaluator

let coreReferences = [ typeof<LispProgram>.Assembly.Location ]

/// Run the REPL, using the reflection-based evaluator.
let runRepl () =
    ReadLine.HistoryEnabled <- true
    printVersion ()

    let options =
        { defaultScriptOptions with
              References = coreReferences }

    evalWith options >> Result.map print |> repl
