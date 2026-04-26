namespace Feersum.CompilerServices

open Feersum.CompilerServices.Text
open Feersum.CompilerServices.Syntax.Parse

/// A workspace manages the loading and parsing of Scheme source documents.
///
/// It provides a stable, ergonomic foundation for the compiler pipeline and for
/// language services (e.g. LSP).  The workspace owns the source registry and
/// a parse cache so that repeated calls to `Workspace.parse` return the cached
/// result until the underlying text is updated.
///
/// Unit-testing / fabricating of source is a first-class citizen: in-memory
/// source text can be loaded with `Workspace.load` using any path string.
type Workspace =
    { Registry: SourceRegistry
      mutable ParseCache: Map<DocId, ParseResult<Syntax.Tree.Program>> }

module Workspace =

    /// Create a new, empty workspace.
    let create () : Workspace =
        { Registry = SourceRegistry.empty ()
          ParseCache = Map.empty }

    /// Load in-memory source text into the workspace.
    ///
    /// Registers the text with the source registry under the given `path` and
    /// returns a stable `DocId` that can be used to refer to the document.
    let load (workspace: Workspace) (path: string) (text: string) : DocId =
        SourceRegistry.register workspace.Registry path text

    /// Update the source text for a document.
    ///
    /// Replaces the text for the given `id` in the source registry and
    /// invalidates the parse cache entry for that document so that the next
    /// call to `Workspace.parse` re-parses from the new text.  The `path`
    /// parameter allows renaming the document at the same time; pass the
    /// original path (from `Workspace.tryGetDocument`) to keep it unchanged.
    let update (workspace: Workspace) (id: DocId) (path: string) (text: string) : unit =
        SourceRegistry.update workspace.Registry id path text
        workspace.ParseCache <- Map.remove id workspace.ParseCache

    /// Update the source text for a document, preserving the original path.
    ///
    /// Convenience wrapper around `Workspace.update` for the common case where
    /// only the text changes and the path stays the same.
    let updateText (workspace: Workspace) (id: DocId) (text: string) : unit =
        match SourceRegistry.tryLookup workspace.Registry id with
        | Some doc -> update workspace id doc.Path text
        | None -> ()

    /// Parse a loaded document.
    ///
    /// Returns `Some result` when the document is known, `None` when the `id`
    /// is not registered in this workspace.  Results are cached: the document
    /// is only re-parsed if its text was updated with `Workspace.update`.
    let parse (workspace: Workspace) (id: DocId) : ParseResult<Syntax.Tree.Program> option =
        match Map.tryFind id workspace.ParseCache with
        | Some result -> Some result
        | None ->
            match SourceRegistry.tryLookup workspace.Registry id with
            | Some doc ->
                let result =
                    readRaw Program doc.Path doc.Text
                    |> ParseResult.map (fun x -> new Syntax.Tree.Program(x, id))

                workspace.ParseCache <- Map.add id result workspace.ParseCache
                Some result
            | None -> None

    /// Look up the `TextDocument` for a registered document.
    ///
    /// Returns `None` for synthetic document IDs and for IDs not registered
    /// in this workspace.
    let tryGetDocument (workspace: Workspace) (id: DocId) : TextDocument option =
        SourceRegistry.tryLookup workspace.Registry id
