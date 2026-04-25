namespace Feersum.LanguageServer

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Server

/// LSP Client Communication Wrapper
///
/// This class holds the notification and request senders provided by the LSP
/// server framework. It is used to proactively communicate with the client
/// e.g. to publish diagnostics or send window messages.
type private FeersumClient(_notifs: ClientNotificationSender, _reqs: ClientRequestSender) =
    inherit LspClient()
