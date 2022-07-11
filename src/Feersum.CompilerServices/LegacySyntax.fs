namespace Feersum.CompilerServices.Syntax

open FParsec
open System.Globalization
open System.Text

open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Text

/// Constant or literal value in the syntax tree
type SyntaxConstant =
    | Number of double
    | Str of string
    | Boolean of bool
    | Character of char

/// Type of nodes in our syntax tree
type AstNodeKind<'t> =
    | Ident of string
    | Constant of SyntaxConstant
    | Dot
    | Form of 't list
    | Seq of 't list
    | Quoted of 't
    | Vector of 't list
    | ByteVector of byte list
    | Error

/// A node in our syntax tree.
type AstNode =
    { Kind: AstNodeKind<AstNode>
      Location: TextLocation }

module private LegacySyntaxDiagnostics =

    let parseError = DiagnosticKind.Create DiagnosticLevel.Error 1 "Parse error"

    let fparsecFailure = DiagnosticKind.Create DiagnosticLevel.Error 2 "FParsec failure"

/// The parser state. Used to collect
type State =
    { Diagnostics: DiagnosticBag }

    member s.Emit pos message =
        s.Diagnostics.Emit LegacySyntaxDiagnostics.parseError (TextLocation.Point(pos)) message

    static member Empty = { Diagnostics = DiagnosticBag.Empty }

module LegacyParse =

    let errorNode =
        { Kind = AstNodeKind.Error
          Location = Point(TextPoint.FromExternal(Position("error", 0L, 0L, 0L))) }

    let expect (parser: Parser<'t, State>) message : Parser<'t option, State> =
        fun stream ->
            let reply = parser stream

            match reply.Status with
            | ReplyStatus.Error ->
                stream.UserState.Emit (TextPoint.FromExternal(stream.Position)) message
                Reply(None)
            | _ -> Reply(reply.Status, Some(reply.Result), reply.Error)

    let skipUnrecognised problem : Parser<unit, State> =
        fun stream ->
            let pos = stream.Position
            let skipped = stream.ReadCharOrNewline()

            if skipped = EOS then
                Reply(ReplyStatus.Error, expected "at end of string")
            else
                stream.UserState.Emit (TextPoint.FromExternal(pos)) (problem skipped)
                Reply(())

    let expectCharWithMessage message chr = expect (skipChar chr) (message chr)

    let expectChar = expectCharWithMessage (sprintf "Expected '%c'")

    let expectCharClosing = expectCharWithMessage (sprintf "Missing closing '%c'")

    let private parseForm, parseFormRef = createParserForwardedToRef ()

    let private spannedNode nodeCons nodeParser =
        getPosition .>>. nodeParser .>>. getPosition
        |>> (fun ((s, i), e) ->
            { Kind = nodeCons (i)
              Location = Span(TextPoint.FromExternal(s), TextPoint.FromExternal(e)) })

    let private spannedNodeOfKind atomKind = spannedNode (fun _ -> atomKind)

    let private comment =
        let singleLine = skipChar ';' >>. skipRestOfLine true
        let datum = skipString "#;" .>> parseForm
        let block, blockRef = createParserForwardedToRef ()

        let blockBody =
            choice [ block
                     skipMany1 (skipNoneOf "|#")
                     attempt (skipChar '|' >>. skipNoneOf "#")
                     skipChar '#' ]

        blockRef.Value <- between (skipString "#|") (skipString "|#") (skipMany blockBody)

        singleLine <|> datum <|> block

    let private ws = skipMany (comment <|> unicodeSpaces1)

    let private parseNum = spannedNode (Number >> Constant) pfloat

    let private unescapedChar = noneOf "\"\\"

    let private hexScalarValue =
        let hexUnescape x =
            System.Int32.Parse(x, NumberStyles.HexNumber)
            |> System.Char.ConvertFromUtf32
            |> System.Char.TryParse

        fun (stream: CharStream<State>) ->
            let s = stream.Position
            let r = many1Chars hex stream

            if r.Status = ReplyStatus.Ok then
                match hexUnescape r.Result with
                | (true, ch) -> Reply(ch)
                | (false, _) ->
                    stream.UserState.Emit (TextPoint.FromExternal(s)) "Invalid code unit"
                    Reply('\uFFFD')
            else
                Reply(r.Status, r.Error)

    let private hexEscape =
        between (skipString "\\x") (expectChar ';') (expect (hexScalarValue) "Expected hex character literal")
        |>> Option.defaultValue '\uFFFD'

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
        between (skipChar '"') (expectCharClosing '"') (manyChars (unescapedChar <|> hexEscape <|> escapedChar))
        |> spannedNode (Str >> Constant)

    let private parseVec =
        between (skipString "#(") (expectCharClosing ')') (many parseForm)
        |> spannedNode Vector

    let private parseByteVec =
        between (skipString "#u8(") (expectCharClosing ')') (many (between (ws) (ws) puint8))
        |> spannedNode ByteVector

    let private parseBool =
        stringReturn "#true" true
        <|> stringReturn "#t" true
        <|> stringReturn "#false" false
        <|> stringReturn "#f" false
        |> spannedNode (Boolean >> Constant)

    let private parseChar =
        let namedChar =
            choice [ stringReturn "alarm" '\u0007'
                     stringReturn "backspace" '\u0008'
                     stringReturn "delete" '\u007F'
                     stringReturn "escape" '\u001B'
                     stringReturn "newline" '\u000A'
                     stringReturn "null" '\u0000'
                     stringReturn "return" '\u000D'
                     stringReturn "space" ' '
                     stringReturn "tab" '\u0009' ]

        let hexChar = attempt (skipChar 'x' >>. hexScalarValue)

        spannedNode
            (Character >> Constant)
            (skipString @"#\"
             >>. (namedChar <|> hexChar <|> anyChar))

    let inline private isIdentifierChar c =
        isAsciiLetter c
        || isDigit c
        || isAnyOf "!$%&*/:<=>?@^_~+-." c

    let private parseIdent =
        let simpleIdent = many1SatisfyL isIdentifierChar "identifier"

        let identLiteralChar = (manyChars ((noneOf "\\|") <|> hexEscape <|> escapedChar))

        let identLiteral = between (skipChar '|') (expectCharClosing '|') identLiteralChar

        spannedNode Ident (simpleIdent <|> identLiteral)

    let private parseDot =
        (skipChar '.'
         >>? notFollowedBy (satisfy isIdentifierChar))
        |> spannedNodeOfKind Dot

    let private parseQuoted = skipAnyOf "’'" >>. parseForm |> spannedNode Quoted

    let private parseAtom =
        // The order is important here. Numbers have higher priority than
        // symbols / identifiers. The `.` token must come before identifier.
        choice [ parseStr
                 parseVec
                 parseByteVec
                 parseChar
                 parseNum
                 parseBool
                 parseDot
                 parseIdent ]

    let private parseApplication =
        between (skipChar '(') (expectCharClosing ')') (many parseForm)
        |> spannedNode Form

    do parseFormRef.Value <- between ws ws (parseApplication <|> parseAtom <|> parseQuoted)

    /// Parse the given string into a syntax tree
    let private parse: Parser<AstNode, State> =
        let problem = sprintf "unexpected character %c"

        (many (
            parseForm
            <|> ((skipUnrecognised problem) >>% errorNode)
        ))
        .>> eof
        |> spannedNode Seq

    /// Unpack a `ParseResult` into a Plain `Result`
    let private unpack =
        function
        | Success (node, s, _) -> (node, s.Diagnostics.Take)
        | Failure (mess, err, s) ->
            s.Diagnostics.Emit LegacySyntaxDiagnostics.fparsecFailure (Point(TextPoint.FromExternal(err.Position))) mess

            s.Diagnostics.Emit
                LegacySyntaxDiagnostics.fparsecFailure
                (Point(TextPoint.FromExternal(err.Position)))
                "The parser encountered an error that could not be recovered from."

            (errorNode, s.Diagnostics.Take)

    /// Read a single expression from the named input text
    let readExpr1 name line : (AstNode * Diagnostic list) =
        runParserOnString parse State.Empty name line
        |> unpack

    /// Read a single expression from the input text
    let readExpr = readExpr1 "repl"

    /// Read an expression from source code on disk
    let parseFile path : (AstNode * Diagnostic list) =
        runParserOnFile parse State.Empty path Encoding.UTF8
        |> unpack

    /// Read an expression from a stream of source code
    let parseStream name stream : (AstNode * Diagnostic list) =
        runParserOnStream parse State.Empty name stream Encoding.UTF8
        |> unpack
