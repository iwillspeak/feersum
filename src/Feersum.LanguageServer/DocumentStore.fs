namespace Feersum.LanguageServer

open System.Collections.Concurrent
open OmniSharp.Extensions.LanguageServer.Protocol

/// Thread-safe store of open document content keyed by document URI.
type DocumentStore() =
    let store = ConcurrentDictionary<DocumentUri, string>()

    /// Open or replace a document in the store.
    member _.Open(uri: DocumentUri, content: string) = store.[uri] <- content

    /// Update the content of an open document.
    member _.Update(uri: DocumentUri, content: string) = store.[uri] <- content

    /// Close a document and remove it from the store.
    member _.Close(uri: DocumentUri) = store.TryRemove(uri) |> ignore

    /// Try to retrieve the content of a document by URI.
    member _.TryGet(uri: DocumentUri) =
        match store.TryGetValue(uri) with
        | true, content -> Some content
        | _ -> None
