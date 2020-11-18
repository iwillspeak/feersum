module Diagnostics

open FParsec

let missingPos = Position("missing", 0L, 0L, 0L)

/// A point in the source text
/// 
/// A text position represents either a single `Point` in the source text that
/// lies 'between' two characters, or a `Span` that encompases a range of text.
type TextLocation =
    | Span of Position * Position
    | Point of Position
    | Missing
with
    /// Get the start of the text location. This returns a cursor that lies just
    /// before any text represented by this locaiton.
    member x.Start =
        match x with
        | Span(s, _) -> s
        | Point p -> p
        | Missing -> missingPos

    /// Get the end of the text location. This returns a cursot that lies just
    /// after any text represented by this location.
    member x.End =
        match x with
        | Span(_, e) -> e
        | Point p -> p
        | Missing -> missingPos

/// Diagnostics indicate problems with our source code at a given position.
type Diagnostic = Diagnostic of TextLocation * string
with
    /// Helper to retrieve the message for this diagnostic.
    member d.Message =
        match d with
        | Diagnostic(_, message) -> message
        
    /// Format the diagnostic for output.
    override d.ToString() =
        match d with
        | Diagnostic(loc, message) ->
            let pos = loc.Start
            sprintf "%s:%d:%d: %s" pos.StreamName pos.Line pos.Column message

/// A collection of diagnostics being built by a compiler phase.
type DiagnosticBag = { mutable Diagnostics: Diagnostic list }
with
    /// Buffer a diagnostic into the bag.
    member b.Emit pos message =
        let d = Diagnostic(pos, message)
        b.Diagnostics <- d::b.Diagnostics

    /// Finalise the bag by taking the diagnostics from it.
    member b.Take = b.Diagnostics

    /// Create a new, empty, diagnostics bag.
    static member Empty =
        { Diagnostics = [] }

/// Test if a given diagnostics collection contains any errors.
let public hasErrors (diagnostics: Diagnostic seq): bool =
    not (Seq.isEmpty diagnostics)