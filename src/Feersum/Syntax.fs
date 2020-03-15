module Syntax

open System
open System.IO
open FParsec

// The main AST Node type
type AstNode =
    | Ident of string
    | Number of int64
    | Str of string
    | Boolean of bool
    | Form of AstNode list
    | Seq of AstNode list

let comment = 
    pchar ';' >>. skipRestOfLine true

let ws = skipMany (comment <|> unicodeSpaces1)

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
    between ws ws (choice [
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