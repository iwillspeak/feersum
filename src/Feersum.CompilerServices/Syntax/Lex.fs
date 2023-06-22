module Feersum.CompilerServices.Syntax.Lex

open System
open System.Text
open Feersum.CompilerServices.Text

/// Token kinds for the language.
type TokenKind =
    | Whitespace = 0
    | OpenBracket = 1
    | CloseBracket = 2
    | Quote = 3
    | Unquote = 4
    | Dot = 5
    | Comment = 6
    | Identifier = 7
    | DatumCommentMarker = 8
    | Number = 9
    | String = 10
    | Character = 11
    | Boolean = 12
    | VectorPrefix = 13
    | BytevectorPrefix = 14
    | Error = 99
    | EndOfFile = 100

/// Token in our lexical pass
type LexicalToken =
    { Kind: TokenKind
      Lexeme: string
      Offset: Firethorn.TextLength }

/// States in our lexer's state machine.
type private LexState =
    | Start
    | SimpleToken of kind: TokenKind
    | ComplexToken of remaining: char list * kind: TokenKind
    | SingleLineComment
    | SeenHash
    | InMultiLine of depth: int
    | InMultiLineSeenHash of depth: int
    | InMultiLineSeenBar of depth: int
    | MultiLineDone
    | Identifier
    | PerculiarIdentifierSeenSign
    | PerculiarIdentifierSeenDot
    | LiteralIdentifier
    | LiteralIdentifierSeenEscape
    | String
    | StringSeenEscape
    | Char
    | CharNamed
    | CharHex
    | Number
    | NumberSuffix
    | Bool of next: char * longSuffix: char list
    | Whitespace
    | Error

// =============================== Utilities ==================================

/// Charcters, other than alphabetics, that can start an identifier.
let private specialInitial =
    [ '!'; '$'; '%'; '&'; '*'; '/'; ':'; '<'; '='; '>'; '?'; '@'; '^'; '_'; '~' ]
    |> Set.ofList

/// Characters that can appear after an explicit sign at the beginning of
/// an identifier.
let private signSubseqent = [ '-'; '+'; '@' ] |> Set.ofList

/// Non-alphanumeric characters that can appear after the start of a plain
/// identifier.
let private specialSubsequent = [ '+'; '-'; '.'; '@' ] |> Set.ofList

/// Hexadecimal digits, in both upper and lower case.
let private hexDigits =
    [ '0'
      '1'
      '2'
      '3'
      '4'
      '5'
      '6'
      '7'
      '8'
      '9'
      'a'
      'b'
      'c'
      'd'
      'e'
      'f'
      'A'
      'B'
      'C'
      'D'
      'E'
      'F' ]
    |> Set.ofList

// =============================== Public API ==================================

/// Tokenise the input text. Returns an enumerable sequence of the tokens within
/// the text.
let public tokenise input =

    let tokenForState lexeme state pos =
        let kind =
            match state with
            | Start -> TokenKind.EndOfFile
            | SimpleToken t -> t
            | SingleLineComment
            | MultiLineDone -> TokenKind.Comment
            | Whitespace -> TokenKind.Whitespace
            | PerculiarIdentifierSeenSign
            | Identifier -> TokenKind.Identifier
            | PerculiarIdentifierSeenDot ->
                // If we are in a 'perculiar identifier' just having seen a dot
                // then we _may_ be parsing `+.`, `-.`, or `.`. Only `.` is not
                // actually an identifier. Handle that case here rather than
                // having two separate states for dot after sign and plain dot.
                if lexeme = "." then TokenKind.Dot else TokenKind.Identifier
            | Number
            | NumberSuffix -> TokenKind.Number
            | Bool _ -> TokenKind.Boolean
            | CharNamed
            | CharHex -> TokenKind.Character
            | ComplexToken _
            | LiteralIdentifier
            | LiteralIdentifierSeenEscape
            | Char
            | StringSeenEscape
            | String
            | Error
            | SeenHash
            | InMultiLine _
            | InMultiLineSeenHash _
            | InMultiLineSeenBar _ -> TokenKind.Error

        { Kind = kind
          Lexeme = lexeme
          Offset = pos }

    /// Get the next transition, if any, for the current `state` and `c`. This
    /// returns `Some` if a transition exists, and `None` if no transition is
    /// available.
    let nextTransition state c =

        match state with
        | Start ->
            match c with
            | c when Char.IsWhiteSpace(c) -> Some(LexState.Whitespace)
            | '(' -> Some(LexState.SimpleToken TokenKind.OpenBracket)
            | ')' -> Some(LexState.SimpleToken TokenKind.CloseBracket)
            | '\''
            | '’'
            | '`' -> Some(LexState.SimpleToken TokenKind.Quote)
            | ',' -> Some(LexState.SimpleToken TokenKind.Unquote)
            | '.' -> Some(LexState.PerculiarIdentifierSeenDot)
            | ';' -> Some(LexState.SingleLineComment)
            | '#' -> Some(LexState.SeenHash)
            | '-'
            | '+' -> Some(LexState.PerculiarIdentifierSeenSign)
            | '"' -> Some(LexState.String)
            | '|' -> Some(LexState.LiteralIdentifier)
            | c when Char.IsLetter(c) || (Set.contains c specialInitial) -> Some(LexState.Identifier)
            | c when Char.IsDigit(c) -> Some(LexState.Number)
            | _ -> Some(LexState.Error)
        | SingleLineComment ->
            match c with
            | '\r'
            | '\n' -> None
            | _ -> Some(LexState.SingleLineComment)
        | SeenHash ->
            match c with
            | '|' -> Some(LexState.InMultiLine 1)
            | ';' -> Some(LexState.SimpleToken TokenKind.DatumCommentMarker)
            | '\\' -> Some(LexState.Char)
            | 't' -> Some(LexState.Bool('r', [ 'u'; 'e' ]))
            | 'f' -> Some(LexState.Bool('a', [ 'l'; 's'; 'e' ]))
            | '(' -> Some(LexState.SimpleToken TokenKind.VectorPrefix)
            | 'u' -> Some(LexState.ComplexToken([ '8'; '(' ], TokenKind.BytevectorPrefix))
            | _ -> None
        | InMultiLine n ->
            match c with
            | '|' -> Some(LexState.InMultiLineSeenBar n)
            | '#' -> Some(LexState.InMultiLineSeenHash n)
            | _ -> Some(LexState.InMultiLine n)
        | InMultiLineSeenHash n ->
            match c with
            | '|' -> Some(LexState.InMultiLine(n + 1))
            | _ -> Some(LexState.InMultiLine n)
        | InMultiLineSeenBar n ->
            match c with
            | '#' ->
                if n > 1 then
                    Some(LexState.InMultiLine(n - 1))
                else
                    Some(LexState.MultiLineDone)
            | _ -> Some(LexState.InMultiLine n)
        | Identifier ->
            if
                Char.IsLetterOrDigit c
                || Set.contains c specialInitial
                || Set.contains c specialSubsequent
            then
                Some(LexState.Identifier)
            else
                None
        | PerculiarIdentifierSeenSign ->
            match c with
            | c when
                Char.IsLetter(c)
                || Set.contains c specialInitial
                || Set.contains c signSubseqent
                ->
                Some(LexState.Identifier)
            | '.' -> Some(LexState.PerculiarIdentifierSeenDot)
            | c when Char.IsDigit(c) -> Some(LexState.Number)
            | _ -> None
        | PerculiarIdentifierSeenDot ->
            match c with
            | '.' -> Some(LexState.Identifier)
            | c when
                Char.IsLetter(c)
                || Set.contains c specialInitial
                || Set.contains c signSubseqent
                ->
                Some(LexState.Identifier)
            | c when Char.IsDigit(c) -> Some(LexState.NumberSuffix)
            | _ -> None
        | Number ->
            if Char.IsDigit(c) then Some(LexState.Number)
            else if c = '.' then Some(LexState.NumberSuffix)
            else None
        | NumberSuffix ->
            if Char.IsDigit(c) then
                Some(LexState.NumberSuffix)
            else
                None
        | String ->
            match c with
            | '\\' -> Some(LexState.StringSeenEscape)
            | '"' -> Some(LexState.SimpleToken TokenKind.String)
            | _ -> Some(LexState.String)
        | StringSeenEscape -> Some(LexState.String)
        | LiteralIdentifier ->
            match c with
            | '\\' -> Some(LexState.LiteralIdentifierSeenEscape)
            | '|' -> Some(LexState.SimpleToken TokenKind.Identifier)
            | _ -> Some(LexState.LiteralIdentifier)
        | LiteralIdentifierSeenEscape -> Some(LexState.LiteralIdentifier)
        | Char ->
            match c with
            | 'x' -> Some(LexState.CharHex)
            | c when Char.IsLetter(c) -> Some(LexState.CharNamed)
            | _ -> Some(LexState.SimpleToken TokenKind.Character)
        | CharHex ->
            if Set.contains c hexDigits then
                Some(LexState.CharHex)
            else
                None
        | CharNamed -> if Char.IsLetter(c) then Some(LexState.CharNamed) else None
        | Bool(next, suffix) ->
            if c = next then
                Some(LexState.ComplexToken(suffix, TokenKind.Boolean))
            else
                None
        | Whitespace ->
            match c with
            | c when Char.IsWhiteSpace(c) -> Some(LexState.Whitespace)
            | _ -> None
        | ComplexToken(remaining, kind) ->
            match remaining with
            | [ single ] -> if c = single then Some(LexState.SimpleToken kind) else None
            | expected :: rest ->
                if c = expected then
                    Some(LexState.ComplexToken(rest, kind))
                else
                    None
            | _ -> None
        // Simple accepting states
        | Error
        | SimpleToken _
        | MultiLineDone -> None

    let mutable state = Start
    let mutable lexeme = StringBuilder()
    // TODO: we should probably stop tracking offset in the tokens and instead
    ///      keep track as we eat their lexemes in the parser.
    let mutable offset = 0

    seq {
        for char in input do
            offset <- offset + 1

            match nextTransition state char with
            | None ->
                yield tokenForState (lexeme.ToString()) state offset
                lexeme <- lexeme.Clear().Append(char)

                state <- nextTransition Start char |> Option.defaultValue Error
            | Some next ->
                state <- next
                lexeme <- lexeme.Append(char)

        if state <> Start then
            yield tokenForState (lexeme.ToString()) state offset
    }
