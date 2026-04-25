namespace Feersum.LanguageServer

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open System

/// LSP Server Implementation
///
/// The server is the main stateful root of the language server. It holds our
/// document store and workspace information and handles the various LSP methods
/// that we support by delegating to the appropriate modules.
[<Sealed>]
type private FeersumServer(client: FeersumClient) =
    inherit LspServer()

    // -- Global state ---------------------------------------------------------

    let workspace = Workspace.create ()
    let compilationManager = CompilationManager.create client workspace

    // -- Lifecycle ------------------------------------------------------------

    override _.Initialize(_: InitializeParams) =
        { InitializeResult.Default with
            Capabilities =
                { ServerCapabilities.Default with
                    TextDocumentSync =
                        Some(
                            U2.C1
                                { TextDocumentSyncOptions.Default with
                                    OpenClose = Some true
                                    Change = Some TextDocumentSyncKind.Full }
                        )
                    DiagnosticProvider =
                        Some(
                            U2.C1
                                { InterFileDependencies = false
                                  Identifier = Some "Feersum Scheme"
                                  WorkspaceDiagnostics = false
                                  WorkDoneProgress = None }
                        )
                    SemanticTokensProvider =
                        Some(
                            U2.C1
                                { WorkDoneProgress = None
                                  Legend =
                                    { TokenTypes = SemanticTokenizer.tokenTypeNames
                                      TokenModifiers = [||] }
                                  Range = None
                                  Full = Some(U2.C1 true) }
                        ) } }
        |> Ok
        |> async.Return

    override _.Dispose() = ()

    // -- Diagnostics ----------------------------------------------------------

    override _.TextDocumentDiagnostic(arg: DocumentDiagnosticParams) =
        async {
            return
                Ok(
                    // We don't support pull diagnostics yet. When a request is
                    // received just return an empty result. We'll push any
                    // diagnostics when we next recompile.
                    U2.C2
                        { Kind = "unchanged"
                          ResultId = Guid.NewGuid().ToString()
                          RelatedDocuments = None }
                )
        }

    // -- Text document sync ---------------------------------------------------

    override _.TextDocumentDidOpen(p: DidOpenTextDocumentParams) =
        async {
            let! _ =
                workspace.PostAndAsyncReply(fun reply ->
                    OpenDocument(p.TextDocument.Uri, p.TextDocument.Text, p.TextDocument.Version, reply))

            compilationManager.Post(DocumentChanged p.TextDocument.Uri)
        }

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

                compilationManager.Post(DocumentChanged p.TextDocument.Uri)
                return ()
        }

    override _.TextDocumentDidClose(p: DidCloseTextDocumentParams) =
        workspace.PostAndAsyncReply(fun reply -> CloseDocument(p.TextDocument.Uri, reply))

    // No need to force a re-compilation on close, since the document is now gone
    // On our next recompilation we'll revert back to the source file on disk.

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
