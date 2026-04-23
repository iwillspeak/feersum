namespace Feersum.LanguageServer

open System.Threading
open System.Threading.Tasks
open OmniSharp.Extensions.LanguageServer.Protocol
open OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities
open OmniSharp.Extensions.LanguageServer.Protocol.Document
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open OmniSharp.Extensions.LanguageServer.Protocol.Server.Capabilities

/// LSP text document sync handler that keeps document content current in a
/// DocumentStore so other handlers can parse on demand.
type FeersumTextDocumentSyncHandler(store: DocumentStore) =
    inherit TextDocumentSyncHandlerBase()

    override _.GetTextDocumentAttributes(uri: DocumentUri) = TextDocumentAttributes(uri, "scheme")

    override _.CreateRegistrationOptions(_: TextSynchronizationCapability, _: ClientCapabilities) =
        let opts = TextDocumentSyncRegistrationOptions(TextDocumentSyncKind.Full)
        opts.DocumentSelector <- TextDocumentSelector.ForLanguage([| "scheme" |])
        opts

    override _.Handle(request: DidOpenTextDocumentParams, _: CancellationToken) =
        store.Open(request.TextDocument.Uri, request.TextDocument.Text)
        Task.FromResult(MediatR.Unit.Value)

    override _.Handle(request: DidChangeTextDocumentParams, _: CancellationToken) =
        match request.ContentChanges |> Seq.tryLast with
        | Some change -> store.Update(request.TextDocument.Uri, change.Text)
        | None -> ()

        Task.FromResult(MediatR.Unit.Value)

    override _.Handle(_: DidSaveTextDocumentParams, _: CancellationToken) = Task.FromResult(MediatR.Unit.Value)

    override _.Handle(request: DidCloseTextDocumentParams, _: CancellationToken) =
        store.Close(request.TextDocument.Uri)
        Task.FromResult(MediatR.Unit.Value)
