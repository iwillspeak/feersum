namespace Feersum.LanguageServer

/// Versioned Document State
///
/// Represents a snapshot of a text document at a given version. The workspace
/// state stores these keyed by the document URI. Agents that depend on document
/// information can query for these with `getDocument` and `getAllDocuments`.
type DocumentState = { Version: int; Text: string }

/// The Workspace State
///
/// This is the main state of the language server. It stores the current
/// open documents. State is managed by the workspace agent.
type private WorkspaceState =
    { Documents: Map<string, DocumentState> }


/// Mailbox Message Type for the Workspace Agent
type WorkspaceMessage =
    | OpenDocument of path: string * text: string * version: int * AsyncReplyChannel<unit>
    | ChangeDocument of path: string * text: string * version: int * AsyncReplyChannel<unit>
    | CloseDocument of path: string * AsyncReplyChannel<unit>
    | GetDocumentSnapshot of path: string * AsyncReplyChannel<DocumentState option>
    | GetAllDocuments of AsyncReplyChannel<Map<string, DocumentState>>

/// The Workspace Agent
///
/// This agent manages the workspace state, which currently consists of the open
/// documents and their contents. It receives messages to open, change, and close
/// documents, as well as queries to get the current state of a document or all
/// documents. This allows us to centralise all state management for the workspace
/// in a single agent, which can be safely accessed from multiple threads without
/// worrying about concurrency issues.
type WorkspaceAgent = MailboxProcessor<WorkspaceMessage>

module Workspace =

    /// Create a new workspace agent
    let create () : WorkspaceAgent =
        MailboxProcessor.Start(fun inbox ->
            let rec loop (state: WorkspaceState) =
                async {
                    let! msg = inbox.Receive()

                    match msg with
                    | OpenDocument(path, text, version, reply) ->
                        let newState =
                            { state with
                                Documents = state.Documents |> Map.add path { Version = version; Text = text } }

                        reply.Reply()
                        return! loop newState
                    | ChangeDocument(path, text, version, reply) ->
                        let newState =
                            { state with
                                Documents = state.Documents |> Map.add path { Version = version; Text = text } }

                        reply.Reply()
                        return! loop newState
                    | CloseDocument(path, reply) ->
                        let newState =
                            { state with
                                Documents = state.Documents |> Map.remove path }

                        reply.Reply()
                        return! loop newState
                    | GetDocumentSnapshot(path, reply) ->
                        let docState = state.Documents |> Map.tryFind path
                        reply.Reply docState
                        return! loop state
                    | GetAllDocuments reply ->
                        reply.Reply state.Documents
                        return! loop state
                }

            loop { Documents = Map.empty })
