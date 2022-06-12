module Feersum.ParseRepl

open System
open Feersum.CompilerServices.Syntax.ParseNew
open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Syntax
open Feersum.CompilerServices.Syntax.TreeNew

let private read () =
    ReadLine.Read("§> ") |> readProgram "repl.scm"

let private print (result: ParseResult) =
    if result.Errors <> [] then
        dumpDiagnostics result.Errors

    TreeNew.dump result.Root

let rec parserReplImpl () =
    read () |> print
    parserReplImpl ()

/// Run the Parser REPL
///
/// This REPL repeatedly parses using the new parser, and then dumps out the
/// resulting tree.
let rec public runParserRepl () =
    ReadLine.HistoryEnabled <- true

    parserReplImpl ()
