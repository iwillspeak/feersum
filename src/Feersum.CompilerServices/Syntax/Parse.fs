module Feersum.CompilerServices.Syntax.Parse

open System.IO

open Firethorn.Green
open Firethorn.Red

open Feersum.CompilerServices.Ice
open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Syntax.Tree

open Lex

/// Private module to contain all the diagnostic kinds that the parser emits.
module private ParserDiagnostics =

    /// Generic parse error diagnostic. This is raised when no more specific
    /// error is available.
    let parseError = DiagnosticKind.Create DiagnosticLevel.Error 10 "Parse error"

/// Type to represent parse results.
///
/// Parser results represent the result of a parser operation. A result
/// always contains some syntax item `Root`, along with a list of
/// `Diagnostics`.
///
/// Parser results can be transformed and consumed with `ParseResult.map`,
/// `ParseResult.toResult`.
type public ParseResult<'a> =
    { Diagnostics: Diagnostic list
      Root: 'a }

module ParseResult =

    /// Check a parse result for errors
    let public hasErrors result = result.Diagnostics |> hasErrors

    /// Map the results of a parse
    ///
    /// Allows the result of a parse to be functionally transformed without
    /// affecting the buffered diagnostics. This is used by the 'typed parse'
    /// APIs to convert untyped syntax trees into their typed wrappers.
    let public map mapper result =
        { Diagnostics = result.Diagnostics
          Root = result.Root |> mapper }

    /// Convert a parser response into a plain result type
    ///
    /// This drops any tree from the error, but opens up parser responses to
    /// being processed using standard error handling
    let public toResult response =
        match response.Diagnostics with
        | [] -> response.Root |> Ok
        | errs -> errs |> Result.Error

/// Parser Type
///
/// Type to hold state when parsing. This is not intended to be used
/// directly. The main parser API is via the `read*` functions.
type private ParserState =
    { Tokens: LexicalToken list
      Diagnostics: Diagnostic list }

module private ParserState =

    /// Create a new parser state from the given sequence of tokens
    let fromTokens tokens =
        { Tokens = List.ofSeq tokens
          Diagnostics = [] }

    /// Bump the token enumerator, returning the current token and new state.
    let bump state =
        match state.Tokens with
        | [] -> None, state
        | head :: rest -> Some(head), { state with Tokens = rest }

    /// Buffer a raw diagnostic at this position
    let bufferDiagnosticRaw state diagnostic =
        { state with ParserState.Diagnostics = (diagnostic :: state.Diagnostics) }

    /// Buffer a diagnostic at the current token in the parser state.
    let bufferDiagnostic state diagKind message =
        let pos =
            match List.tryHead state.Tokens with
            | Some token -> token.Location
            | _ -> Missing

        Diagnostic.Create diagKind pos message
        |> bufferDiagnosticRaw state

    /// Finalise the parse state
    let finalise (builder: GreenNodeBuilder) rootKind state =
        let root =
            builder.BuildRoot(rootKind |> SyntaxUtils.astToGreen)
            |> SyntaxNode.CreateRoot

        { Diagnostics = state.Diagnostics
          Root = root }


/// Type to represent different kinds of programs we can read
type public ReadMode =
    | Program
    | Script

// =========================== Parser Utilities ===============================

/// Get the kind of the current token. If no more tokens remain then EOF is
/// returned instead.
let private currentKind state =
    match List.tryHead state.Tokens with
    | Some token -> token.Kind
    | _ -> TokenKind.EndOfFile

/// Check if the parser state is currently at a token of the given `kind`.
let private lookingAt kind state = currentKind state = kind

/// Check if the parser state is currently at a token with any of `kinds`
let private lookingAtAny kinds state = List.contains (currentKind state) kinds

/// Eat a single token as the given `kind`
let private eat (builder: GreenNodeBuilder) kind state =
    let (token, state) = ParserState.bump state

    let lexeme =
        match token with
        | Some token -> token.Lexeme
        | None -> ""

    builder.Token(kind |> SyntaxUtils.astToGreen, lexeme)
    state

/// Expect a given token kind, or buffer a diagnostic otherwise.
let private expect (builder: GreenNodeBuilder) tokenKind nodeKind state =

    if lookingAt tokenKind state then
        eat builder nodeKind state
    else
        sprintf "Expected %A, got %A" tokenKind (currentKind state)
        |> ParserState.bufferDiagnostic state ParserDiagnostics.parseError
        |> eat builder AstKind.ERROR

// =============================== Parsers ===================================

let private parseConstant (builder: GreenNodeBuilder) state =
    builder.StartNode(AstKind.CONSTANT |> SyntaxUtils.astToGreen)

    let mutable state = state

    let kind =
        match currentKind state with
        | TokenKind.String -> AstKind.STRING
        | TokenKind.Number -> AstKind.NUMBER
        | TokenKind.Boolean -> AstKind.BOOLEAN
        | TokenKind.Character -> AstKind.CHARACTER
        | _ ->
            state <-
                sprintf "Unexpected token %A" (currentKind state)
                |> ParserState.bufferDiagnostic state ParserDiagnostics.parseError

            AstKind.ERROR

    let state = eat builder kind state

    builder.FinishNode()
    state

let private skipAtmosphere (builder: GreenNodeBuilder) state =
    let mutable state = state

    while lookingAtAny
              [ TokenKind.Whitespace
                TokenKind.Comment ]
              state do
        state <- eat builder AstKind.ATMOSPHERE state

    state

let private parseIdentifier (builder: GreenNodeBuilder) state =
    builder.StartNode(AstKind.SYMBOL |> SyntaxUtils.astToGreen)
    let state = expect builder TokenKind.Identifier AstKind.IDENTIFIER state
    builder.FinishNode()
    state

let rec private parseQuote (builder: GreenNodeBuilder) state =
    builder.StartNode(AstKind.QUOTED_DATUM |> SyntaxUtils.astToGreen)

    let state =
        state
        |> expect builder TokenKind.Quote AstKind.QUOTE
        |> parseExpr builder

    builder.FinishNode()
    state

and private parseAtom builder state =
    match currentKind state with
    | TokenKind.Identifier -> parseIdentifier builder state
    | TokenKind.Quote -> parseQuote builder state
    | _ -> parseConstant builder state

and private parseFormTail builder state =
    let mutable state = state

    while not (
        lookingAtAny
            [ TokenKind.EndOfFile
              TokenKind.CloseBracket ]
            state
    ) do
        state <- parseExpr builder state

    let state = expect builder TokenKind.CloseBracket AstKind.CLOSE_PAREN state
    builder.FinishNode()

    state

and private parseForm (builder: GreenNodeBuilder) state =
    builder.StartNode(AstKind.FORM |> SyntaxUtils.astToGreen)

    expect builder TokenKind.OpenBracket AstKind.OPEN_PAREN state
    |> parseFormTail builder


and private parseVec (builder: GreenNodeBuilder) state =
    builder.StartNode(AstKind.VEC |> SyntaxUtils.astToGreen)

    expect builder TokenKind.VectorPrefix AstKind.OPEN_PAREN state
    |> parseFormTail builder

and private parseBytevec (builder: GreenNodeBuilder) state =
    builder.StartNode(AstKind.BYTEVEC |> SyntaxUtils.astToGreen)

    // TODO: Strinctly speaking this should be many numbers, not many
    //       expressions. We can always deal with that later on in semantic
    //       analys however. It might be worth changing this though. Datums are
    //       more restrictive than plain expressions. We might benefit from
    //       better modelling the grammar non-terminals of the spec in general.
    //       e.g. (quote <datum>) and '<datum> being the same node kind in the
    //       tree.
    expect builder TokenKind.BytevectorPrefix AstKind.OPEN_PAREN state
    |> parseFormTail builder

and private parseExpr builder state =
    match currentKind state with
    | TokenKind.OpenBracket -> parseForm builder state
    | TokenKind.VectorPrefix -> parseVec builder state
    | TokenKind.BytevectorPrefix -> parseBytevec builder state
    | _ -> parseAtom builder state
    |> skipAtmosphere builder

and private parseExprSeq builder endKinds state =
    if lookingAtAny endKinds state then
        state
    else
        state
        |> parseExpr builder
        |> skipAtmosphere builder
        |> parseExprSeq builder endKinds

/// Parse Program
///
/// Read a sequence of expressions as a single program.
let private parseProgram (builder: GreenNodeBuilder) state : ParseResult<SyntaxNode> =
    skipAtmosphere builder state
    |> parseExprSeq builder [ TokenKind.EndOfFile ]
    |> ParserState.finalise builder AstKind.PROGRAM

/// Parse Expression
///
/// Reads a single expression from the lexer
let private parseScript (builder: GreenNodeBuilder) state : ParseResult<SyntaxNode> =
    skipAtmosphere builder state
    |> parseExpr builder
    |> ParserState.finalise builder AstKind.SCRIPT_PROGRAM

// =============================== Public API ==================================

/// Read a raw syntax tree from the given input.
let readRaw mode name (line: string) =
    let parseState = Lex.tokenise line name |> ParserState.fromTokens

    let builder = GreenNodeBuilder()

    match mode with
    | Program -> parseProgram builder parseState
    | Script -> parseScript builder parseState

/// Read a sequence of expressions as a program from the given `input`.
let readProgram name input =
    readRaw Program name input
    |> ParseResult.map (fun x -> new Program(x))

/// Read a single expression from the named input `line`.
let readExpr1 name line =
    readRaw Script name line
    |> ParseResult.map (fun x -> new ScriptProgram(x))

/// Read a single expression from the input `line` using an implicit name.
let readExpr = readExpr1 "repl"

/// Read an expression from source code on disk
let parseFile path =
    async {
        let! text = File.ReadAllTextAsync(path) |> Async.AwaitTask
        return readProgram path text
    }
