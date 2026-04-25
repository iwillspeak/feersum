namespace Feersum.LanguageServer

type CompilationMessage =
    | RecompileAll
    | DocumentChanged of path: string

module private CompilationManager =
    open Feersum.CompilerServices.Text
    open Feersum.CompilerServices.Syntax
    open Feersum.CompilerServices.Syntax.Parse
    open Feersum.CompilerServices.Diagnostics
    open Ionide.LanguageServerProtocol
    open Ionide.LanguageServerProtocol.Types

    // -- Diagnostic conversion ------------------------------------------------

    /// Convert a 1-based Feersum TextPoint to a 0-based LSP Position.
    let private toPosition (pt: TextPoint) : Position =
        // FIXME: The `unit32` casts here hint that we support _ridiculously sized_
        // documents. We should probably clamp these and instead just support plain
        // uin32 offsets in Feersum just like LSP.
        { Line = uint32 (pt.Line - 1)
          Character = uint32 (pt.Col - 1) }

    /// Convert a Feersum TextLocation to an LSP Range.
    let private toRange (loc: TextLocation) : Range =
        { Start = loc.Start |> toPosition
          End = loc.End |> toPosition }

    /// Convert a Feersum DiagnosticLevel to an LSP DiagnosticSeverity.
    let private toSeverity (level: DiagnosticLevel) : DiagnosticSeverity =
        match level with
        | DiagnosticLevel.Error -> DiagnosticSeverity.Error
        | DiagnosticLevel.Warning -> DiagnosticSeverity.Warning

    let private toCodeDescription (title: string) : CodeDescription option =
        // We can optionally provide a URL here that gives more information about the
        // diagnostic. For now we just link to the Feersum documentation homepage.
        Some { Href = "https://docs.feersum-scheme.net/compiler-reference/error-index/" }

    /// Convert a single Feersum Diagnostic to an LSP Diagnostic.
    let private toLspDiagnostic (diag: Feersum.CompilerServices.Diagnostics.Diagnostic) : Diagnostic =
        { Range = toRange diag.Location
          Severity = Some(toSeverity diag.Kind.Level)
          Code = U2.C2(sprintf "SCM%04u" diag.Kind.Code) |> Some
          CodeDescription = toCodeDescription diag.Kind.Title
          Source = Some "Feersum Compiler"
          Message = diag.Message
          Tags = None
          RelatedInformation = None
          Data = None }

    /// Build LSP PublishDiagnosticsParams for a single document.
    let private toPublishParams
        (uri: string)
        (version: int)
        (diags: Feersum.CompilerServices.Diagnostics.Diagnostic list)
        : PublishDiagnosticsParams =
        { Uri = uri
          Version = Some version
          Diagnostics = diags |> List.map toLspDiagnostic |> Array.ofList }

    // -- Agent ----------------------------------------------------------------

    let create (client: FeersumClient) (workspace: WorkspaceAgent) : MailboxProcessor<CompilationMessage> =
        MailboxProcessor.Start(fun inbox ->
            let rec loop (state: unit) =
                async {
                    let! msg = inbox.Receive()

                    match msg with
                    // Handle both single document changes and full recompiles
                    // the same way for now.
                    | DocumentChanged _
                    | RecompileAll ->
                        let! documents = workspace.PostAndAsyncReply(fun reply -> GetAllDocuments reply)
                        let sourceRegistry = SourceRegistry.empty ()

                        eprintfn "Recompiling %d documents..." (documents |> Map.count)

                        for path, docState in documents |> Map.toSeq do
                            let result = Parse.readProgram sourceRegistry path docState.Text
                            let p = toPublishParams path docState.Version result.Diagnostics
                            do! client.TextDocumentPublishDiagnostics(p)

                        return! loop state
                }

            loop ())
