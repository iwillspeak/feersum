module Feersum.ParseRepl

open ReadLineReboot
open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Syntax.Tree
open Feersum.CompilerServices.Syntax.Parse
open Feersum.CompilerServices.Text

let private read () =
    ReadLine.Read("[]> ") |> readProgram "repl.scm"

let private print (result: ParseResult<Program>) =
    if ParseResult.hasErrors result then
        dumpDiagnostics result.Diagnostics

    SyntaxUtils.dump result.Root.RawNode

let private parserReplImpl () =
    let rec loop () =
        read () |> ParseResult.map _.Item |> print
        loop ()

    loop ()

/// Run the Parser REPL
///
/// This REPL repeatedly parses using the new parser, and then dumps out the
/// resulting tree.
let rec public runParserRepl () =
    ReadLine.HistoryEnabled <- true

    parserReplImpl ()
