namespace Feersum.LanguageServer

open System
open Firethorn.Red
open Feersum.CompilerServices.Syntax.Parse
open Feersum.CompilerServices.Syntax.Tree
open Feersum.CompilerServices.Text

// -- Measure types ------------------------------------------------------------

/// A token type is a 0-based index into the token legend.
[<Measure>]
type TokenType

/// A token modifier is a bitmask flag into the token modifiers legend.
[<Measure>]
type TokenModifier

// -- Semantic tokeniser -------------------------------------------------------

module SemanticTokenizer =
    open Feersum.CompilerServices.Binding

    // -- LSP token legend -----------------------------------------------------
    let private tokenComment = 0<TokenType>, "comment"
    let private tokenKeyword = 1<TokenType>, "keyword"
    let private tokenNumber = 2<TokenType>, "number"
    let private tokenOperator = 3<TokenType>, "operator"
    let private tokenString = 4<TokenType>, "string"
    let private tokenVariable = 5<TokenType>, "variable"
    let private tokenEnum = 6<TokenType>, "enum"

    let private tokens =
        [| tokenComment
           tokenKeyword
           tokenNumber
           tokenOperator
           tokenString
           tokenVariable
           tokenEnum |]

    /// The ordered list of token type names, used to build the LSP legend.
    let tokenTypeNames = tokens |> Array.map snd

    /// Map an AstKind to a 0-based token type index, returning None to skip.
    let private kindToTokenType (text: string) (kind: AstKind) : int<TokenType> option =
        match kind with
        | AstKind.ATMOSPHERE when text.Length > 0 && not (Char.IsWhiteSpace(text[0])) -> Some(fst tokenComment)
        | AstKind.BOOLEAN -> Some(fst tokenEnum)
        | AstKind.CHARACTER
        | AstKind.STRING -> Some(fst tokenString)
        | AstKind.NUMBER -> Some(fst tokenNumber)
        | AstKind.IDENTIFIER ->
            match Stx.resolve text Map.empty with
            | Some(StxBinding.Special _) -> Some(fst tokenKeyword)
            | _ -> Some(fst tokenVariable)
        | AstKind.QUOTE
        | AstKind.DOT -> Some(fst tokenOperator)
        | _ -> None

    /// Parse `content` at `path` and return all single-line semantic tokens
    /// encoded as a flat uint32 array in the LSP 5-integer-per-token delta
    /// format: deltaLine, deltaStartChar, length, tokenTypeIndex, modifiers.
    let tokenize (path: string) (content: string) : uint32[] =
        let registry = SourceRegistry.empty ()
        let result = readProgram registry path content

        match SourceRegistry.tryLookup registry result.Root.DocId with
        | None -> [||]
        | Some(doc: TextDocument) ->
            let acc = ResizeArray<uint32>()
            let mutable prevLine = 0
            let mutable prevCol = 0
            let mutable datumCommentDepth = 0

            for event in Walk.walk result.Root.RawNode do
                match event with
                | EnterNode n ->
                    if n.Kind |> SyntaxUtils.greenToAst = AstKind.ATMOSPHERE then
                        datumCommentDepth <- datumCommentDepth + 1
                | LeaveNode n ->
                    if n.Kind |> SyntaxUtils.greenToAst = AstKind.ATMOSPHERE then
                        datumCommentDepth <- datumCommentDepth - 1
                | OnToken token ->
                    let text = token.Green.Text

                    let kind =
                        if datumCommentDepth > 0 then
                            AstKind.ATMOSPHERE
                        else
                            token.Kind |> SyntaxUtils.greenToAst

                    match kindToTokenType text kind with
                    // Multi-line tokens are skipped: LSP delta encoding requires
                    // each token to reside on a single line.
                    | Some tokenType when not (text.Contains('\n')) ->
                        let pt = TextDocument.offsetToPoint doc token.Range.Start
                        // offsetToPoint returns 1-based line/col; convert to 0-based.
                        let line = pt.Line - 1
                        let col = pt.Col - 1
                        let length = token.Range.End - token.Range.Start

                        let deltaLine = line - prevLine
                        let deltaCol = if deltaLine = 0 then col - prevCol else col

                        acc.Add(uint32 deltaLine)
                        acc.Add(uint32 deltaCol)
                        acc.Add(uint32 length)
                        acc.Add(uint32 (int tokenType))
                        acc.Add(0u) // no modifiers

                        prevLine <- line
                        prevCol <- col
                    | _ -> ()

            acc.ToArray()
