module Feersum.ParseRepl

open ReadLineReboot
open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Syntax.Tree
open Feersum.CompilerServices.Syntax.Parse
open Feersum.CompilerServices.Text

let private read registry docId =
    ReadLine.Read("[]> ") |> readProgramAt registry docId "repl.scm"

let private print (result: ParseResult<Program>) =
    if ParseResult.hasErrors result then
        dumpDiagnostics result.Diagnostics

    SyntaxUtils.dump result.Root.RawNode

let private parserReplImpl registry =
    let docId = SourceRegistry.register registry "repl.scm" ""

    let rec loop () =
        read registry docId |> print
        loop ()

    loop ()

/// Run the Parser REPL
///
/// This REPL repeatedly parses using the new parser, and then dumps out the
/// resulting tree.
let rec public runParserRepl () =
    ReadLine.HistoryEnabled <- true

    parserReplImpl (SourceRegistry.empty ())
