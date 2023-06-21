module Feersum.ParseRepl

open System
open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Syntax.Tree
open Feersum.CompilerServices.Syntax
open Feersum.CompilerServices.Syntax.Parse
open Feersum.CompilerServices.Text

let private read () =
    ReadLine.Read("[]> ") |> Parse.readProgram "repl.scm"

let private print (result: ParseResult<Program>) =
    if ParseResult.hasErrors result then
        dumpDiagnostics result.Diagnostics

    SyntaxUtils.dump result.Root.RawNode

    let doc = TextDocument.fromParts "repl.scm" ""

    printfn "Transformed: %A" (SyntaxShim.transformProgram doc result.Root)

let rec private parserReplImpl () =
    read () |> print
    parserReplImpl ()

/// Run the Parser REPL
///
/// This REPL repeatedly parses using the new parser, and then dumps out the
/// resulting tree.
let rec public runParserRepl () =
    ReadLine.HistoryEnabled <- true

    parserReplImpl ()
