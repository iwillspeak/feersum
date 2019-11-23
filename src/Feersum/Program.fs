// Learn more about F# at http://fsharp.org

open System

module Syntax =
    // The main AST Node type
    type AstNode =
        | Atom of string
        | Number of int
        | Str of string
        | Boolean of bool

    /// Parse the given string into a syntax tree
    let parse input =
        Str input

    /// Read a single line of user input and parse it into a
    /// syntax tree.
    let public read (): AstNode =
        Console.Write "§> "
        Console.Out.Flush()
        parse(Console.ReadLine())

open Syntax

/// Shceme value
///
/// Represents the different types that a given value can have
/// in the interpreter.
type SchemeValue =
    | Nil
    | Number of int
    | Str of string
    | Boolean of bool

/// Take a syntax tree and evaluate it producing a value.
let execute (input: AstNode) =
    match input with
    | AstNode.Number n  -> SchemeValue.Number n
    | AstNode.Str s -> SchemeValue.Str s
    | AstNode.Boolean b -> SchemeValue.Boolean b
    | _ -> SchemeValue.Nil

/// Print a value out to the console
let print value =
    value |> printfn "]= %A"

/// Read, Execute, Print Loop
///
/// Repeatedly reads input and prints output
let rec repl () =
    (read()
    // |> execute
    |> print)
    repl()

[<EntryPoint>]
let main argv =
    repl()
    0 // return an integer exit code
