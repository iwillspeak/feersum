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
type private FeersumServer(_client: FeersumClient) =
    inherit LspServer()

    // -- Document store -------------------------------------------------------

    let workspace = Workspace.create ()

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
        workspace.PostAndAsyncReply(fun reply ->
            OpenDocument(p.TextDocument.Uri, p.TextDocument.Text, p.TextDocument.Version, reply))

    override _.TextDocumentDidChange(p: DidChangeTextDocumentParams) =
        async {
            match p.ContentChanges with
            | [||] -> ()
            | changes ->
                let text =
                    match Array.last changes with
                    | U2.C1 c -> c.Text
                    | U2.C2 c -> c.Text

                let! _ =
                    workspace.PostAndAsyncReply(fun reply ->
                        ChangeDocument(p.TextDocument.Uri, text, p.TextDocument.Version, reply))

                return ()
        }

    override _.TextDocumentDidClose(p: DidCloseTextDocumentParams) =
        workspace.PostAndAsyncReply(fun reply -> CloseDocument(p.TextDocument.Uri, reply))

    // -- Semantic tokens ------------------------------------------------------

    override _.TextDocumentSemanticTokensFull(p: SemanticTokensParams) =
        let uri = p.TextDocument.Uri

        async {
            let! docState = workspace.PostAndAsyncReply(fun reply -> GetDocumentSnapshot(uri, reply))

            match docState with
            | None -> return Error(JsonRpc.Error.InvalidRequest "Document not found in workspace")
            | Some content ->
                let data = SemanticTokenizer.tokenize uri content.Text
                return Ok(Some { ResultId = None; Data = data })
        }
