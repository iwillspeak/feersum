// Learn more about F# at http://fsharp.org

open System

// The main AST Node type
type AstNode =
    | Atom of String
    | Number of int
    | Str of String
    | Boolean of bool

/// Read a single line of user input and parse it into a
/// syntax tree.
let read () =
    Console.Write "§> "
    Console.Out.Flush()
    Console.ReadLine()

/// Take a syntax tree and evaluate it producing a value.
let execute input =
    input

/// Print a value out to the console
let print value =
    value |> printfn "]= %A"


/// Read, Execute, Print Loop
///
/// Repeatedly reads input and prints output
let rec repl () =
    (read() |> execute |> print)
    repl()

[<EntryPoint>]
let main argv =
    repl()
    0 // return an integer exit code
