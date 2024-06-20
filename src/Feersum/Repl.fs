module Feersum.Repl

open ReadLineReboot

open Feersum.Version
open Feersum.CompilerServices.Syntax
open Feersum.CompilerServices.Eval
open Feersum.CompilerServices.Diagnostics
open Feersum.Core
open Feersum.CompilerServices.Compile
open Feersum.CompilerServices.Text
open Feersum.CompilerServices.Syntax.Parse

/// Read a single line of user input and parse it into a
/// syntax tree. If the input can't be parsed then read
/// again.
let rec private read () : CompileInput =
    let rec readWithState prompt previous =
        let line = ReadLine.Read(prompt)
        let source =
            match previous with
            | Some prefix -> prefix + "\n" + line
            | None -> line

        match Parse.readExpr1 "repl" source |> ParseResult.toResult with
        | Result.Ok tree -> CompileInput.Script(TextDocument.fromParts "repl" source, tree) |> Ok
        | Result.Error diagnostics ->
            if line = "" && source.EndsWith("\n\n") then
                Result.Error(diagnostics)
            else
                readWithState "+> " (Some(source))
            
    match readWithState "§> " None with
    | Result.Ok input -> input
    | Result.Error diagnostics ->
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
    with ex ->
        eprintfn "Exception: %A" ex

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
