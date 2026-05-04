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
        let line = ReadLine.Read prompt

        let source =
            match previous with
            | Some prefix -> prefix + "\n" + line
            | None -> line

        let parsed = Parse.readExpr1 "repl" source

        if ParseResult.hasErrors parsed then
            if line = "" && source.EndsWith("\n\n") then
                Error parsed.Diagnostics
            else
                readWithState "+> " (Some source)
        else
            CompileInput.Script parsed.Root |> Ok

    match readWithState "§> " None with
    | Ok input -> input
    | Error diagnostics ->
        diagnostics |> dumpDiagnostics
        read ()

/// Print an object out to the console. Used to serialise the external
/// representation form an eval
let private print value =
    value |> cilExternalRepr |> printfn "}= %s"

let coreReferences = [ typeof<LispProgram>.Assembly.Location ]

/// Run the REPL, using the reflection-based evaluator.
///
/// Each input is compiled as a script derived from the previous step so that
/// top-level definitions and imports accumulate across lines.
let runRepl () =
    ReadLine.HistoryEnabled <- true
    printVersion ()

    use initCtx =
        EvalContext.create
            { defaultScriptOptions with
                References = coreReferences }

    let rec loop ctx =
        let nextCtx =
            try
                let result, next = evalInContext ctx (read ())

                match result with
                | Ok value -> print value
                | Error diags -> dumpDiagnostics diags

                next
            with ex ->
                eprintfn "Exception: %A" ex
                ctx

        loop nextCtx

    loop initCtx
