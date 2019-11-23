// Learn more about F# at http://fsharp.org

open System

module Syntax =
    open FParsec

    // The main AST Node type
    type AstNode =
        | Ident of string
        | Number of int64
        | Str of string
        | Boolean of bool
        | Form of AstNode list
        | Seq of AstNode list

    let parseNum =
        pint64 |>> Number
    
    let unescapedChar =
        noneOf "\"\\"
    
    let escapedChar =
        let inline unescape ch =
            match ch with
            | 'n' -> '\n'
            | 't' -> '\t'
            | c -> c
        pchar '\\' >>. anyChar |>> unescape
          
    let parseStr =
        let lit = between (pchar '"') (pchar '"')
                    (manyChars (unescapedChar <|> escapedChar))
        lit |>> Str

    let parseBool =
        stringReturn "#t" (Boolean true) <|>
        stringReturn "#f" (Boolean false)
    
    let parseIdent =
        let isNumChar = isAnyOf "+-."
        let inline isAsciiExtended c =
            isAsciiLetter c || isDigit c || isAnyOf "!$%&*/:<=>?@^_~" c
        let isAsciiIdContinue c =
            isNumChar c || isAsciiExtended c
        (choice [
            identifier(IdentifierOptions(isAsciiIdContinue = isAsciiIdContinue,
                                         isAsciiIdStart = isAsciiExtended))
            pstring "-"
            pstring "+" 
        ]) |>> Ident

    let parseAtom =
        choice [
            parseStr
            parseNum
            parseBool
            parseIdent
        ]

    let parseForm, parseFormRef = createParserForwardedToRef()

    let parseApplication =
        between (pchar '(') (pchar ')')
            ((many parseForm) |>> Form)
           
    do parseFormRef :=
        between spaces spaces (choice [
            parseApplication
            parseAtom
        ])

    /// Parse the given string into a syntax tree
    let parse =
        (many parseForm) |>> Seq

    /// Read a single line of user input and parse it into a
    /// syntax tree. If the input can't be parsed then read
    /// again.
    let rec public read (): AstNode =
        Console.Write "§> "
        Console.Out.Flush()
        let line = Console.ReadLine()
        match (run parse line) with
        | Success(node, _, _) -> node
        | Failure(message, _, _) -> 
            (printfn "Failure: %s" message)
            read()

open Syntax

let printTree tree =
    printfn "%A" tree
    tree

/// Shceme value
///
/// Represents the different types that a given value can have
/// in the interpreter.
type SchemeValue =
    | Nil
    | Number of int64
    | Str of string
    | Boolean of bool
    | Func of ((SchemeValue list) -> SchemeValue)
    | Quoted of AstNode

let apply value args =
    match value with
    | Func f -> f(args)
    | _ -> Nil

/// Take a syntax tree and evaluate it producing a value.
let rec execute (input: AstNode): SchemeValue =
    match input with
    | AstNode.Number n  -> SchemeValue.Number n
    | AstNode.Str s -> SchemeValue.Str s
    | AstNode.Boolean b -> SchemeValue.Boolean b
    | AstNode.Seq exprs -> exprs |> List.map execute |> List.tryLast |> Option.defaultValue SchemeValue.Nil
    | AstNode.Form([AstNode.Ident "quote"; e]) -> SchemeValue.Quoted e
    | AstNode.Form([AstNode.Ident "if"; cond; ifTrue; ifFalse]) ->
        match (execute cond) with
        | SchemeValue.Boolean false -> execute ifFalse
        | _ -> execute ifTrue
    | AstNode.Form(head::rest) -> apply (execute head) (List.map execute rest)
    | _ -> SchemeValue.Nil

/// Print a value out to the console
let print value =
    value |> printfn "]= %A"

/// Read, Execute, Print Loop
///
/// Repeatedly reads input and prints output
let rec repl () =
    (read()
    |> printTree
    |> execute
    |> print)
    repl()

[<EntryPoint>]
let main argv =
    repl()
    0 // return an integer exit code
