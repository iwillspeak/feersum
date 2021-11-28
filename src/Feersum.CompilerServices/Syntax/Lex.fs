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
    member _.Done = tokenStart < buffer.Length |> not

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
            | Identifier -> TokenKind.Identifier
            | Error
            | SeenHash
            | InMultiLine _ 
            | InMultiLineSeenHash _ 
            | InMultiLineSeenBar _ -> TokenKind.Error

        let tokenValue = buffer.[tokenStart..currentChar]
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

        let specialSubsequent = [ '+'; '-'; '.'; '@' ] |> Set.ofList

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
            | '.' -> Some(LexState.SimpleToken TokenKind.Dot)
            | ';' -> Some(LexState.SingleLineComment)
            | '#' -> Some(LexState.SeenHash)
            | c when
                Char.IsLetter(c)
                || (Set.contains c specialInitial)
                ->
                Some(LexState.Identifier)
            | _ -> Some(LexState.Error)
        | SingleLineComment ->
            match c with
            | '\r'
            | '\n' -> None
            | _ -> Some(LexState.SingleLineComment)
        | SeenHash ->
            match c with
            | '|' -> Some(LexState.InMultiLine 1)
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
        | Whitespace ->
            match c with
            | c when Char.IsWhiteSpace(c) -> Some(LexState.Whitespace)
            | _ -> None
        // Simple accepting states
        | Error
        | SimpleToken _
        | MultiLineDone -> None
