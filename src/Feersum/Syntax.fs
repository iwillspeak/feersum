module Syntax

open FParsec
open System.Globalization
open System.Text

open Diagnostics

/// Type of nodes in our syntax tree
type AstNodeKind<'t> =
    | Ident of string
    | Number of float
    | Str of string
    | Boolean of bool
    | Character of char
    | Dot
    | Form of 't list
    | Seq of 't list
    | Quoted of 't
    | Error

/// A node in our syntax tree.
type AstNode = { Kind: AstNodeKind<AstNode>
               ; Location: TextLocation }

/// The parser state. Used to collect diagnostics
type State = { Diagnostics: DiagnosticBag }
with
    member s.Emit pos message =
        s.Diagnostics.Emit (TextLocation.Point(pos)) message

    static member Empty =
        { Diagnostics = DiagnosticBag.Empty }

let errorNode = { Kind = AstNodeKind.Error; Location = Point(Position("error", 0L, 0L, 0L))}

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

let private spannedNode nodeCons nodeParser =
    getPosition .>>. nodeParser .>>. getPosition
    |>> (fun ((s, i), e) -> { Kind = nodeCons(i); Location = Span(s, e) })

let private spannedNodeOfKind atomKind =
    spannedNode (fun _ -> atomKind)

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
    spannedNode Number pfloat

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
    between (skipChar '"') (skipChar '"')
                (manyChars (unescapedChar <|> hexEscape <|> escapedChar))
    |> spannedNode Str

let private parseBool =
    stringReturn "#true"  true <|>
    stringReturn "#t"     true <|>
    stringReturn "#false" false <|>
    stringReturn "#f"     false
    |> spannedNode Boolean

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
    spannedNode Character (skipString @"#\" >>. (namedChar <|> hexChar <|> anyChar))

let inline private isIdentifierChar c =
    isAsciiLetter c || isDigit c || isAnyOf "!$%&*/:<=>?@^_~+-." c

let private parseIdent =
    let simpleIdent = many1SatisfyL isIdentifierChar "identifier"
    let identLiteralChar = (manyChars ((noneOf "\\|") <|> hexEscape <|> escapedChar))
    let identLiteral = between (skipChar '|') (skipChar '|') identLiteralChar

    spannedNode Ident (simpleIdent <|> identLiteral)
 
let private parseDot =
    (skipChar '.' >>? notFollowedBy (satisfy isIdentifierChar))
    |> spannedNodeOfKind Dot

let private parseQuoted =
    skipChar '\'' >>. parseForm |> spannedNode Quoted

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
        (many parseForm)
    |> spannedNode Form

do parseFormRef :=
    between ws ws (parseApplication <|> parseAtom <|> parseQuoted)

/// Parse the given string into a syntax tree
let private parse: Parser<AstNode, State> =
    let problem =
        sprintf "unexpected character %c"
    (many (parseForm <|> ((skipUnrecognised problem) >>% errorNode))) .>> eof
    |> spannedNode Seq

/// Unpack a `ParseResult` into a Plain `Result`
let private unpack = function
    | Success(node, s, _) -> (node, s.Diagnostics.Take)
    | Failure(mess, err, s) ->
        s.Diagnostics.Emit (Point(err.Position)) mess
        (errorNode, s.Diagnostics.Take)

/// Read expressions from the input text
let readExpr line: (AstNode * Diagnostic list) =
    runParserOnString parse State.Empty "repl" line |> unpack

/// Read an expression from source code on disk
let parseFile path: (AstNode * Diagnostic list)=
    runParserOnFile parse State.Empty path Encoding.UTF8 |> unpack

/// Read an expression from a stream of source code
let parseStream name stream: (AstNode * Diagnostic list) =
    runParserOnStream parse State.Empty name stream Encoding.UTF8 |> unpack