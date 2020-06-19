module Syntax

open System.IO
open FParsec
open System.Text

// The main AST Node type
type AstNode =
    | Ident of string
    | Number of float
    | Str of string
    | Boolean of bool
    | Form of AstNode list
    | Seq of AstNode list

let private comment = 
    pchar ';' >>. skipRestOfLine true

let private ws = skipMany (comment <|> unicodeSpaces1)

let private parseNum =
    pfloat |>> Number

let private unescapedChar =
    noneOf "\"\\"

let private escapedChar =
    let inline unescape ch =
        match ch with
        | 'n' -> '\n'
        | 't' -> '\t'
        | c -> c
    pchar '\\' >>. anyChar |>> unescape
      
let private parseStr =
    let lit = between (pchar '"') (pchar '"')
                (manyChars (unescapedChar <|> escapedChar))
    lit |>> Str

let private parseBool =
    stringReturn "#t" (Boolean true) <|>
    stringReturn "#f" (Boolean false)

let private parseIdent =
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

let private parseAtom =
    choice [
        parseStr
        parseNum
        parseBool
        parseIdent
    ]

let private parseForm, parseFormRef = createParserForwardedToRef()

let private parseApplication =
    between (pchar '(') (pchar ')')
        ((many parseForm) |>> Form)
       
do parseFormRef :=
    between ws ws (choice [
        parseApplication
        parseAtom
    ])

/// Parse the given string into a syntax tree
let private parse =
    (many parseForm) .>> eof |>> Seq

/// Unpack a `ParseResult` into a Plain `Result`
let private unpack = function
    | Success(node, _, _) -> Result.Ok node
    | Failure(message, _, _) -> Result.Error message

/// Read expressions from the input text
let readExpr line: Result<AstNode,string> =
    (run parse line) |> unpack

/// Read an expression from source code on disk
let parseFile path =
    runParserOnFile parse () path Encoding.UTF8 |> unpack

/// Read an expression from a stream of source code
let parseStream name stream =
    runParserOnStream parse () name stream Encoding.UTF8 |> unpack