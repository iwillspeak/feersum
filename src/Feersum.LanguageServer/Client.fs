namespace Feersum.LanguageServer

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Server

/// LSP Client Communication Wrapper
///
/// This class holds the notification and request senders provided by the LSP
/// server framework. It is used to proactively communicate with the client
/// e.g. to publish diagnostics or send window messages.
type private FeersumClient(notifs: ClientNotificationSender, _reqs: ClientRequestSender) =
    inherit LspClient()

    override _.TextDocumentPublishDiagnostics(arg: Types.PublishDiagnosticsParams) : Async<unit> =
        async {
            eprintfn
                "Publishing diagnostics for %s (version %A) with %d diagnostics"
                arg.Uri
                arg.Version
                (arg.Diagnostics |> Array.length)

            let! _ = notifs "textDocument/publishDiagnostics" arg
            return ()
        }
