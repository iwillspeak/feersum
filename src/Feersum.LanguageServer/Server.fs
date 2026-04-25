namespace Feersum.LanguageServer

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types

/// LSP Server Implementation
///
/// The server is the main stateful root of the language server. It holds our
/// document store and workspace information and handles the various LSP methods
/// that we support by delegating to the appropriate modules.
[<Sealed>]
type private FeersumServer(_: FeersumClient) =
    inherit LspServer()

    // -- Document store -------------------------------------------------------

    let mutable documentStore: Map<string, string> = Map.empty

    // -- Lifecycle ------------------------------------------------------------

    override _.Initialize(_: InitializeParams) =
        { InitializeResult.Default with
            Capabilities =
                { ServerCapabilities.Default with
                    TextDocumentSync =
                        { TextDocumentSyncOptions.Default with
                            OpenClose = Some true
                            Change = Some TextDocumentSyncKind.Full }
                        |> U2.C1
                        |> Some
                    SemanticTokensProvider =
                        { WorkDoneProgress = None
                          Legend =
                            { TokenTypes = SemanticTokenizer.tokenTypeNames
                              TokenModifiers = [||] }
                          Range = None
                          Full = Some(U2.C1 true) }
                        |> U2.C1
                        |> Some } }
        |> Ok
        |> async.Return

    override _.Dispose() = ()

    // -- Text document sync ---------------------------------------------------

    override _.TextDocumentDidOpen(p: DidOpenTextDocumentParams) =
        documentStore <- documentStore |> Map.add p.TextDocument.Uri p.TextDocument.Text
        async.Return()

    override _.TextDocumentDidChange(p: DidChangeTextDocumentParams) =
        match p.ContentChanges with
        | [||] -> ()
        | changes ->
            let text =
                match Array.last changes with
                | U2.C1 c -> c.Text
                | U2.C2 c -> c.Text

            documentStore <- documentStore |> Map.add p.TextDocument.Uri text

        async.Return()

    override _.TextDocumentDidClose(p: DidCloseTextDocumentParams) =
        documentStore <- documentStore |> Map.remove p.TextDocument.Uri
        async.Return()

    // -- Semantic tokens ------------------------------------------------------

    override _.TextDocumentSemanticTokensFull(p: SemanticTokensParams) =
        let uri = p.TextDocument.Uri

        match documentStore |> Map.tryFind uri with
        | None -> async.Return(Ok None)
        | Some content ->
            let data = SemanticTokenizer.tokenize uri content
            async.Return(Ok(Some { ResultId = None; Data = data }))
