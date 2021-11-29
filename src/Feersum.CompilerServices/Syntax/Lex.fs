namespace Feersum.Syntax

open System

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
    | Error = 99
    | EndOfFile = 100

/// States in our lexer's state machine.
type private LexState =
    | Start
    | SimpleToken of TokenKind
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
    | Whitespace
    | Error

/// The lexical analyser. This tokenises the input buffer and exposes the
/// `Current` token. The next token can be retrieved by calling `Bump`.
type Lexer(input: string) =

    // The input text as a string
    let buffer = input

    let mutable tokenStart = 0

    let mutable current = None

    // Retrieve the current token.
    member self.Current =
        match current with
        | Some token -> token
        | None ->
            let nextToken = self.GetNextToken()
            current <- Some(nextToken)
            nextToken

    /// Advance past the current token.
    member _.Bump() =
        current <- None
        ()

    /// Check if there are more tokens the lexer could produce.
    member self.Done =
        if tokenStart < buffer.Length then
            false
        else
            let (kind, _) = self.Current
            kind = TokenKind.EndOfFile

    /// Attempt to advance the lexer to another token. No further tokens are
    /// available then `TokenKind.EndOfFile` is always returned.
    member private self.GetNextToken() =

        let mutable state = LexState.Start
        let mutable currentChar = tokenStart
        let mutable tokenEnd = currentChar
        let mutable finished = false

        while not finished && currentChar < buffer.Length do

            let c = buffer.[currentChar]

            let nextState = self.NextTransition(state, c)

            match nextState with
            | Some next ->
                state <- next
                tokenEnd <- currentChar
                currentChar <- currentChar + 1
            | None -> finished <- true

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
                // Slices in F# are _inclusive_ this is effectively checking if
                // the identifier is ony a single `.` character.
                if tokenEnd = tokenStart then
                    TokenKind.Dot
                else
                    TokenKind.Identifier
            | Number -> TokenKind.Number
            | CharNamed
            | CharHex -> TokenKind.Character
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

        let tokenValue = buffer.[tokenStart..tokenEnd]
        tokenStart <- currentChar

        (kind, tokenValue)

    /// Get the next transition, if any, for the current `state` and `c`. This
    /// returns `Some` if a transition exists, and `None` if no transition is
    /// available.
    member private _.NextTransition(state: LexState, c: char) =
        let specialInitial =
            [ '!'
              '$'
              '%'
              '&'
              '*'
              '/'
              ':'
              '<'
              '='
              '>'
              '?'
              '^'
              '_'
              '~' ]
            |> Set.ofList

        let signSubseqent = [ '-'; '+'; '@' ] |> Set.ofList

        let specialSubsequent = [ '+'; '-'; '.'; '@' ] |> Set.ofList

        let hexDigits =
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
            | c when
                Char.IsLetter(c)
                || (Set.contains c specialInitial)
                ->
                Some(LexState.Identifier)
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
            if Char.IsLetterOrDigit c
               || Set.contains c specialInitial
               || Set.contains c specialSubsequent then
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
            | _ -> None
        | Number ->
            if Char.IsDigit(c) then
                Some(LexState.Number)
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
        | CharNamed ->
            if Char.IsLetter(c) then
                Some(LexState.CharNamed)
            else
                None
        | Whitespace ->
            match c with
            | c when Char.IsWhiteSpace(c) -> Some(LexState.Whitespace)
            | _ -> None
        // Simple accepting states
        | Error
        | SimpleToken _
        | MultiLineDone -> None
