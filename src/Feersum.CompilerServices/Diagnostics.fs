namespace Feersum.CompilerServices.Diagnostics

open FParsec
open System.IO

[<AutoOpen>]
module private Internals =
    let missingPos = Position("missing", 0L, 0L, 0L)

/// A point in the source text
///
/// A text position represents either a single `Point` in the source text that
/// lies 'between' two characters, or a `Span` that encompases a range of text.
type TextLocation =
    | Span of Position * Position
    | Point of Position
    | Missing
    /// Get the start of the text location. This returns a cursor that lies just
    /// before any text represented by this locaiton.
    member x.Start =
        match x with
        | Span (s, _) -> s
        | Point p -> p
        | Missing -> missingPos

    /// Get the end of the text location. This returns a cursot that lies just
    /// after any text represented by this location.
    member x.End =
        match x with
        | Span (_, e) -> e
        | Point p -> p
        | Missing -> missingPos

/// Level of diagnostic. Used to tell warnings from errors.
type DiagnosticLevel =
    /// The diagnostic represents an error. Compilation should not continue.
    | Error
    /// THe diagnostic represents a warning. Compilation may continue but the
    /// result may be unexpected or invalid.
    | Warning

/// Diagnostics indicate problems with our source code at a given position.
type Diagnostic =
    { Location: TextLocation
      Message: string
      Level: DiagnosticLevel }

    /// Create a new error diagnostic at the given `location`.`
    static member Create location message =
        Diagnostic.CreateWithKind DiagnosticLevel.Error location message

    /// Create a new warning diagnostic at the given `location`.`
    static member CreateWarning location message =
        Diagnostic.CreateWithKind DiagnosticLevel.Warning location message

    /// Create a new diagnostic with the given `kind` and `location`.
    static member CreateWithKind kind location message =
        { Level = kind
          Location = location
          Message = message }

    /// Format the diagnostic for output.
    override d.ToString() =
        let normaliseName (stream: string) =
            if Path.IsPathRooted(stream) then
                stream
            else
                Path.Join(Directory.GetCurrentDirectory(), stream)

        match d.Location with
        | Missing -> sprintf "feersum: %s: %s" d.MessagePrefix d.Message
        | Point p ->
            sprintf "%s(%d,%d): %s: %s" (p.StreamName |> normaliseName) p.Line p.Column d.MessagePrefix d.Message
        | Span (s, e) ->
            sprintf
                "%s(%d,%d,%d,%d): %s: %s"
                (s.StreamName |> normaliseName)
                s.Line
                s.Column
                e.Line
                e.Column
                d.MessagePrefix
                d.Message

    /// Prefix for the message. Used to summarise the diagnostic kind.
    member private d.MessagePrefix =
        // TODO: proper error codes here rather than hardocding it to SCM9999.
        match d.Level with
        | Warning -> "warning SCM9999"
        | Error -> "error SCM9999"

/// A collection of diagnostics being built by a compiler phase.
type DiagnosticBag =
    { mutable Diagnostics: Diagnostic list }

    /// Buffer a diagnostic into the bag.
    member b.Emit pos message = Diagnostic.Create pos message |> b.Add

    /// Add a diagnostic to the bag.
    member b.Add diag = b.Diagnostics <- diag :: b.Diagnostics

    /// Append a collection of diagnostics to this bag.
    member b.Append diags =
        for d in diags do
            b.Add d

    /// Finalise the bag by taking the diagnostics from it.
    member b.Take = b.Diagnostics

    /// Create a new, empty, diagnostics bag.
    static member Empty = { Diagnostics = [] }

[<AutoOpen>]
module Diagnostics =

    /// Returns true if the diagnostic should be considered an error
    let public isError =
        function
        | { Level = Error } -> true
        | _ -> false

    /// Test if a given diagnostics collection contains any errors.
    let public hasErrors (diagnostics: Diagnostic seq) : bool = Seq.exists (isError) diagnostics

    /// Write the diagnostics to the standard error
    let dumpDiagnostics diags =
        diags
        |> List.rev
        |> Seq.iter (fun x -> eprintfn "%s" (x.ToString()))
