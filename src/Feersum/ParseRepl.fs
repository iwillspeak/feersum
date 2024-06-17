module Feersum.ParseRepl

open System
open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Syntax
open Feersum.CompilerServices.Syntax.Parse

let private read () =
    ReadLine.Read("§> ") |> Parse.readProgram "repl.scm"

let private print result =
    if ParseResult.hasErrors result then
        dumpDiagnostics result.Errors

    Tree.dump result.Root

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
