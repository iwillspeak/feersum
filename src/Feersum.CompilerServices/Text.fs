namespace Feersum.CompilerServices.Text

open System.Collections

/// A point in the source text
///
/// This represents a resolved position within a source. It is used to model
/// locations for diagnostics and other "human facing" features. For general
/// locations within the compiler we use `Firethorn.TextRange` which is
/// a simple byte offset.
type public TextPoint =
    { Source: string
      Line: int
      Col: int }

    static member public FromParts(source: string, line: int, col: int) =
        { Source = source
          Line = line
          Col = col }

/// A location in the source text
///
/// A text position represents either a single `Point` in the source text that
/// lies 'between' two characters, or a `Span` that encompases a range of text.
type public TextLocation =
    | Span of TextPoint * TextPoint
    | Point of TextPoint
    | Missing

    /// Get the start of the text location. This returns a cursor that lies just
    /// before any text represented by this locaiton.
    member x.Start =
        match x with
        | Span(s, _) -> s
        | Point p -> p
        | Missing -> TextPoint.FromParts("missing", 0, 0)

    /// Get the end of the text location. This returns a cursot that lies just
    /// after any text represented by this location.
    member x.End =
        match x with
        | Span(_, e) -> e
        | Point p -> p
        | Missing -> TextPoint.FromParts("missing", 0, 0)

/// A document
type public TextDocument = { Path: string; LineStarts: int list }

module public TextDocument =

    let private lineStarts body =
        body
        |> Seq.indexed
        |> Seq.choose (fun (idx, ch) -> if ch = '\n' then Some(idx) else None)
        |> List.ofSeq

    let public fromParts path body =
        { Path = path
          LineStarts = lineStarts body }

    /// Turn a character offset into a document into a human line column value.
    /// That is a 0-based character offset into the file becomes a 1-based pair
    /// of line and column.
    let private offsetToLineCol lines offset =
        let lineIdx, colIdx =
            match List.tryFindIndexBack (fun x -> x < offset) lines with
            | Some idx ->
                // The `idx` line break occurs before this line. If tis is the
                // break at index 0, we're on the line at index 1.
                idx + 1, offset - lines[idx] - 1
            | None ->
                // no linebreak occurs before the offset, so we must be on the
                // first line.
                0, offset

        lineIdx + 1, colIdx + 1

    let public offsetToPoint document offset =
        let line, col = offsetToLineCol document.LineStarts offset
        TextPoint.FromParts(document.Path, line, col)

    let public rangeToLocation document (range: Firethorn.TextRange) =
        let s = range.Start
        let e = range.End
        Span(s |> offsetToPoint document, e |> offsetToPoint document)

/// A provenance entry describing the origin of syntax
///
/// Currently entries track the source text document. Later variants will
/// describe macro template expansions, substitutions, and other transformations.
type public ProvenanceEntry = SourceText of TextDocument

/// A unique identifier referencing a provenance entry
///
/// Positive IDs index into the ProvenanceTable. Negative IDs represent synthetic
/// syntax that was introduced by transformations (macros, etc.) and are not
/// associated with source text.
type public ProvenanceId = private ProvenanceId of int32

module public ProvenanceId =
    let private syntheticCounter = ref -1

    /// Get the numeric value of a provenance ID
    let public value (ProvenanceId id) = id

    /// Create a synthetic provenance ID for compiler-generated syntax
    let public makeSynthetic () =
        let id = System.Threading.Interlocked.Decrement syntheticCounter
        ProvenanceId id

/// A table tracking the provenance (origin) of all syntax nodes
///
/// Each entry in the table describes where a particular piece of syntax came from.
/// Provenance IDs are indices into this table, allowing multiple nodes to share
/// the same origin while keeping them immutable.
type ProvenanceTable =
    { entries: Generic.List<ProvenanceEntry> }

module public ProvenanceTable =
    /// Create an empty provenance table
    let public empty () =
        { entries = new Generic.List<ProvenanceEntry>() }

    /// Register a new provenance entry and return its ID
    let public register (table: ProvenanceTable) (entry: ProvenanceEntry) : ProvenanceId =
        let id = ProvenanceId table.entries.Count
        table.entries.Add entry
        id

    /// Register a source text origin
    let public registerSourceText (table: ProvenanceTable) (doc: TextDocument) : ProvenanceId =
        register table (SourceText doc)

    /// Look up a provenance entry by ID
    let public lookup (table: ProvenanceTable) (id: ProvenanceId) : ProvenanceEntry option =
        let idx = ProvenanceId.value id |> int

        if idx >= 0 && idx < table.entries.Count then
            table.entries[idx] |> Some
        else
            None
