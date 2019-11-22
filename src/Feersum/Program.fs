// Learn more about F# at http://fsharp.org

open System

// The main AST Node type
type AstNode =
    | Atom of String
    | Number of int
    | Str of String
    | Boolean of bool

[<EntryPoint>]
let main argv =
    Atom "hello world" |> printfn "%A"
    0 // return an integer exit code
