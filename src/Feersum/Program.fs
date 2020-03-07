// Learn more about F# at http://fsharp.org

open System
open System.IO

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

    /// Read expressions from the input text
    let readExpr line: Result<AstNode,string> =
        match (run parse line) with
        | Success(node, _, _) -> Result.Ok node
        | Failure(message, _, _) -> Result.Error message
    
    /// Read a single line of user input and parse it into a
    /// syntax tree. If the input can't be parsed then read
    /// again.
    let rec public read (): AstNode =
        Console.Write "§> "
        Console.Out.Flush()
        let line = Console.ReadLine()
        match readExpr line with
        | Result.Ok node -> node
        | Result.Error message -> 
            (eprintfn "Failure: %s" message)
            read()

    let parseFile path =
        File.ReadAllText path |> readExpr

open Syntax

module SchemeRuntime =

    /// Shceme value
    ///
    /// Represents the different types that a given value can have
    /// in the interpreter.
    type SchemeValue =
        | Nil
        | Number of int64
        | Str of string
        | Boolean of bool
        | Builtin of (SchemeValue list -> SchemeValue)
        | Quoted of AstNode

    let numberBinop op =
        fun values ->
            values |> List.reduce (fun a b ->
                match (a, b) with
                | (Number n, Number m) -> Number (op n m)
                | _ -> Nil)

    let apply value args =
        match value with
        | Builtin f -> f(args)
        | _ -> Nil
     
    let lookup ident =
        match ident with
        | "+" -> Builtin (numberBinop(+))
        | "-" -> Builtin (numberBinop(-))
        | "*" -> Builtin (numberBinop(*))
        | "/" -> Builtin (numberBinop(/))
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
        | AstNode.Form([]) -> SchemeValue.Nil
        | AstNode.Ident i -> lookup i

open SchemeRuntime

module Compile =

    type Symbol = int32

    type BoundExpr =
        | Boolean of bool
        | Number of int64
        | Str of string
        | Form of BoundExpr list
        | Nil

    let bindIdent scope id =
        failwithf "Unimplemented"

    let rec bind scope node =
        match node with
        | AstNode.Number n -> BoundExpr.Number n
        | AstNode.Str s -> BoundExpr.Str s
        | AstNode.Boolean b -> BoundExpr.Boolean b
        | AstNode.Form f -> bindForm scope f
        | AstNode.Ident id -> bindIdent scope id
        | AstNode.Seq s -> bindSequence scope s
    and bindSequence scope exprs =
        List.map (fun f -> bind scope f) exprs |> List.tryLast |> Option.defaultValue BoundExpr.Nil
    and bindForm scope form =
        let boundForm = List.map (fun f -> bind scope f) form
        BoundExpr.Form boundForm
    
    /// Compile a single AST node into an assembly
    ///
    /// The plan for this is we make multiple passes over the syntax tree. First
    /// pass will be to `bind` theh tree. Resulting in a `BoundExpr`. This will
    /// attach any type information that _can_ be computed to each node, and
    /// resolve variable references to the symbols that they refer to.
    /// 
    /// Once the expression is bound we will then `lower` the expression to
    /// flatten it out into a list of blocks. Once we have all the blocks
    /// they can be written out to an `Assembly` with `emit`.
    let compile (node: AstNode) =
        let bound = bind Map.empty node
        bound

    let compileFile (path) =
        match parseFile path with
        | Ok ast -> compile ast
        | Error e -> failwithf "error: %s" e

/// Take a Scheme Value and convert it to the
/// 'external representation' as a string
let externalRepr value =
    match value with
    | Nil -> "NIL"
    | Number n -> n.ToString("d")
    | Str s -> sprintf "%A" s
    | Boolean b -> if b then "#t" else "#f"
    | Builtin f -> "#[procedure]"
    | Quoted q -> sprintf "%A" q

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
