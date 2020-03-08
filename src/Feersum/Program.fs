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

[<EntryPoint>]
let main argv =
    match argv with
    | [| |] -> repl()
    | _ -> Seq.map Compile.compileFile argv |> ignore
    0 // return an integer exit code
