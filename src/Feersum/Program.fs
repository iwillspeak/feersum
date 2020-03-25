// Learn more about F# at http://fsharp.org

open System
open System.IO

open Syntax
open Interpret
open Compile

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
