// Learn more about F# at http://fsharp.org

open Syntax
open System
open Interpret
open Compile

/// Read a single line of user input and parse it into a
/// syntax tree. If the input can't be parsed then read
/// again.
let rec read (): AstNode =
    Console.Write "§> "
    Console.Out.Flush()
    let line = Console.ReadLine()
    match readExpr line with
    | Result.Ok node -> node
    | Result.Error message -> 
        (eprintfn "Failure: %s" message)
        read()

/// Print a value out to the console
let print value =
    value |> externalRepr |> printfn "]= %s"

/// Read, Execute, Print Loop
///
/// Repeatedly reads input and prints output
let rec repl () =
    (read()
    |> execute
    |> print)
    repl()

/// Compile a single file printing an error if
/// there is one.
let compileSingle path =
    match compileFile path with
    | Ok _ -> ()
    | Error e -> failwithf "error: %s" e

[<EntryPoint>]
let main argv =
    match argv with
    | [| |] -> repl()
    | _ -> Seq.iter compileSingle argv
    0 // return an integer exit code
