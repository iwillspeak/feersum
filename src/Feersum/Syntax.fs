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

/// The parser state. Used to collect diagnostics
type State = { mutable Diagnostics: Diagnostic list }
with
    member s.Emit pos message =
        let d = Diagnostic(pos, message)
        s.Diagnostics <- d::s.Diagnostics

    static member Empty =
        { Diagnostics = [] }

let expect (parser: Parser<'t, State>) message: Parser<'t option, State> =
    fun stream ->
        let reply = parser stream
        match reply.Status with
        | ReplyStatus.Error ->
            stream.UserState.Emit stream.Position message
            Reply(None)
        | _ ->
            Reply(reply.Status, Some(reply.Result), reply.Error)

let skipUnrecognised problem: Parser<unit, State> =
    fun stream ->
        let pos = stream.Position
        let skipped = stream.ReadCharOrNewline()
        if skipped = EOS then
            Reply(ReplyStatus.Error, expected "at end of string")
        else
            stream.UserState.Emit pos (problem skipped)
            Reply(())

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
    between (skipChar '(') (expect (skipChar ')') "Missing closing ')'")
        ((many parseForm) |>> Form)
       
do parseFormRef :=
    between ws ws (parseApplication <|> parseAtom)

/// Parse the given string into a syntax tree
let private parse =
    let problem =
        sprintf "unexpected character %c"
    (many (parseForm <|> ((skipUnrecognised problem) >>% AstNode.Error))) .>> eof |>> Seq

/// Unpack a `ParseResult` into a Plain `Result`
let private unpack = function
    | Success(node, s, _) -> (node, s.Diagnostics)
    | Failure(mess, err, s) -> (AstNode.Error, Diagnostic(err.Position, mess)::s.Diagnostics)

/// Read expressions from the input text
let readExpr line: (AstNode * Diagnostic list) =
    runParserOnString parse State.Empty "repl" line |> unpack

/// Read an expression from source code on disk
let parseFile path: (AstNode * Diagnostic list)=
    runParserOnFile parse State.Empty path Encoding.UTF8 |> unpack

/// Read an expression from a stream of source code
let parseStream name stream: (AstNode * Diagnostic list) =
    runParserOnStream parse State.Empty name stream Encoding.UTF8 |> unpack