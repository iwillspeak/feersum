namespace Feersum.LanguageServer

open System.Threading
open System.Threading.Tasks
open Firethorn.Red
open Feersum.CompilerServices.Syntax.Parse
open Feersum.CompilerServices.Syntax.Tree
open Feersum.CompilerServices.Text
open OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities
open OmniSharp.Extensions.LanguageServer.Protocol.Document
open OmniSharp.Extensions.LanguageServer.Protocol.Models

// Alias to disambiguate from OmniSharp's TextDocument model.
type private FsTextDocument = Feersum.CompilerServices.Text.TextDocument

// -------------------------------------------------------------------------
// Internal token classification
// -------------------------------------------------------------------------

module private SemanticTokenizer =

    /// Map an AstKind to a semantic token type string, returning None to skip.
    let kindToTokenType (text: string) (kind: AstKind) =
        match kind with
        | AstKind.ATMOSPHERE when text.Length > 0 && (text.StartsWith(";") || text.StartsWith("#|")) -> Some "comment"
        | AstKind.BOOLEAN -> Some "keyword"
        | AstKind.CHARACTER
        | AstKind.STRING -> Some "string"
        | AstKind.NUMBER -> Some "number"
        | AstKind.IDENTIFIER -> Some "variable"
        | AstKind.QUOTE
        | AstKind.DOT -> Some "operator"
        | _ -> None

    /// Walk a syntax tree root and push single-line semantic tokens into the builder.
    let tokenize (builder: SemanticTokensBuilder) (doc: FsTextDocument) (root: SyntaxNode) =
        for event in Walk.walk root do
            match event with
            | OnToken token ->
                let text = token.Green.Text
                let kind = token.Kind |> SyntaxUtils.greenToAst

                match kindToTokenType text kind with
                // Multi-line tokens (e.g., multi-line strings or block comments) are
                // skipped: the LSP semantic tokens delta encoding requires each pushed
                // token to be on a single line.
                | Some tokenType when not (text.Contains('\n')) ->
                    let start =
                        Feersum.CompilerServices.Text.TextDocument.offsetToPoint doc token.Range.Start

                    let length = token.Range.End - token.Range.Start
                    builder.Push(start.Line - 1, start.Col - 1, length, tokenType, [||])
                | _ -> ()
            | _ -> ()

// -------------------------------------------------------------------------
// Shared legend
// -------------------------------------------------------------------------

/// Semantic token legend shared between registration options and document creation.
/// Token types must be listed here in the same order they are referenced by index.
[<AutoOpen>]
module private SharedLegend =

    let private tokenTypeNames =
        [| "comment"; "keyword"; "number"; "operator"; "string"; "variable" |]

    let legend =
        SemanticTokensLegend(
            TokenTypes = Container.From<SemanticTokenType>(tokenTypeNames |> Array.map SemanticTokenType),
            TokenModifiers = Container.From<SemanticTokenModifier>([||])
        )

// -------------------------------------------------------------------------
// LSP handler
// -------------------------------------------------------------------------

/// Semantic tokens handler that backs the LSP semantic tokens API with the
/// Feersum syntax tree. Supports full-document token delivery.
type FeersumSemanticTokensHandler(store: DocumentStore) =
    inherit SemanticTokensHandlerBase()

    override _.CreateRegistrationOptions(_: SemanticTokensCapability, _: ClientCapabilities) =
        SemanticTokensRegistrationOptions(
            DocumentSelector = TextDocumentSelector.ForLanguage([| "scheme" |]),
            Legend = legend,
            Full = BooleanOr<SemanticTokensCapabilityRequestFull>(true)
        )

    override _.GetSemanticTokensDocument(_: ITextDocumentIdentifierParams, _: CancellationToken) =
        Task.FromResult(SemanticTokensDocument(legend))

    override _.Tokenize
        (builder: SemanticTokensBuilder, ``params``: ITextDocumentIdentifierParams, _: CancellationToken)
        : Task =
        let uri = ``params``.TextDocument.Uri

        match store.TryGet(uri) with
        | None -> ()
        | Some content ->
            let path = uri.ToString()
            let registry = SourceRegistry.empty ()
            let result = readProgram registry path content

            match SourceRegistry.tryLookup registry result.Root.DocId with
            | None -> ()
            | Some(doc: FsTextDocument) -> SemanticTokenizer.tokenize builder doc result.Root.RawNode

        Task.CompletedTask
