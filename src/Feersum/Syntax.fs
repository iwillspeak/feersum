module Syntax

open System.IO
open FParsec
open System.Text
open System.Globalization

/// Diagnostics indicate problems with our source code at a given position.
type Diagnostic = Diagnostic of Position * string
with
    override d.ToString() =
        match d with
        | Diagnostic(pos, message) ->
            sprintf "%s:%d:%d: %s" pos.StreamName pos.Line pos.Column message

/// The main AST Node type
type AstNode =
    | Ident of string
    | Number of float
    | Str of string
    | Boolean of bool
    | Character of char
    | Dot
    | Form of AstNode list
    | Seq of AstNode list
    | Error

let private parseForm, parseFormRef = createParserForwardedToRef()

let private comment =
    let singleLine = skipChar ';' >>. skipRestOfLine true
    let datum = skipString "#;" .>> parseForm
    let block, blockRef = createParserForwardedToRef()
    let blockBody = choice [
        block
        skipMany1 (skipNoneOf "|#")
        attempt (skipChar '|' >>. skipNoneOf "#")
        skipChar '#'
    ]
    blockRef := between (skipString "#|") (skipString "|#") (skipMany blockBody)
    singleLine <|> datum <|> block

let private ws = skipMany (comment <|> unicodeSpaces1)

let private parseNum =
    pfloat |>> Number

let private unescapedChar =
    noneOf "\"\\"

let private hexScalarValue =
    let hexUnescape x =
        System.Int32.Parse(x, NumberStyles.HexNumber)
        |> System.Char.ConvertFromUtf32
        |> System.Char.Parse
    many1Chars hex |>> hexUnescape

let private hexEscape =
    between (skipString "\\x") (skipChar ';') hexScalarValue

let private escapedChar =
    let inline unescape ch =
        match ch with
        | 'a' -> '\a'
        | 'b' -> '\b'
        | 't' -> '\t'
        | 'n' -> '\n'
        | 'v' -> '\v'
        | 'f' -> '\f'
        | 'r' -> '\r'
        | c -> c
    skipChar '\\' >>. (noneOf "x") |>> unescape
      
let private parseStr =
    let lit = between (skipChar '"') (skipChar '"')
                (manyChars (unescapedChar <|> hexEscape <|> escapedChar))
    lit |>> Str

let private parseBool =
    stringReturn "#true"  (Boolean true) <|>
    stringReturn "#t"     (Boolean true) <|>
    stringReturn "#false" (Boolean false) <|>
    stringReturn "#f"     (Boolean false)

let private parseChar =
    let namedChar = choice [
        stringReturn "alarm" '\u0007'
        stringReturn "backspace" '\u0008'
        stringReturn "delete" '\u007F'
        stringReturn "escape" '\u001B'
        stringReturn "newline" '\u000A'
        stringReturn "null" '\u0000'
        stringReturn "return" '\u000D'
        stringReturn "space" ' '
        stringReturn "tab" '\u0009'
    ]
    let hexChar = attempt (skipChar 'x' >>. hexScalarValue)
    skipString @"#\" >>. (namedChar <|> hexChar <|> anyChar) |>> Character

let inline private isIdentifierChar c =
    isAsciiLetter c || isDigit c || isAnyOf "!$%&*/:<=>?@^_~+-." c

let private parseIdent =
    let simpleIdent = many1SatisfyL isIdentifierChar "identifier"
    let identLiteralChar = (manyChars ((noneOf "\\|") <|> hexEscape <|> escapedChar))
    let identLiteral = between (skipChar '|') (skipChar '|') identLiteralChar
    simpleIdent <|> identLiteral |>> Ident
 
let private parseDot =
    (skipChar '.' >>? notFollowedBy (satisfy isIdentifierChar)) >>% Dot 

let private parseAtom =
    // The order is important here. Numbers have higher priority than
    // symbols / identifiers. The `.` token must come before identifier.
    choice [
        parseStr
        parseChar
        parseNum
        parseBool
        parseDot
        parseIdent
    ]

let private parseApplication =
    between (skipChar '(') (skipChar ')')
        ((many parseForm) |>> Form)
       
do parseFormRef :=
    between ws ws (parseApplication <|> parseAtom)

/// Parse the given string into a syntax tree
let private parse =
    (many parseForm) .>> eof |>> Seq

/// Unpack a `ParseResult` into a Plain `Result`
let private unpack = function
    | Success(node, _, _) -> (node, [])
    | Failure(mess, err, _) -> (AstNode.Error, [ Diagnostic(err.Position, mess) ])

/// Read expressions from the input text
let readExpr line: (AstNode * Diagnostic list) =
    runParserOnString parse () "repl" line |> unpack

/// Read an expression from source code on disk
let parseFile path: (AstNode * Diagnostic list)=
    runParserOnFile parse () path Encoding.UTF8 |> unpack

/// Read an expression from a stream of source code
let parseStream name stream: (AstNode * Diagnostic list) =
    runParserOnStream parse () name stream Encoding.UTF8 |> unpack