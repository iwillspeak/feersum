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

/// A document — owns path and line-start offsets for offset → line/col resolution.
type public TextDocument = { Path: string; LineStarts: int list }

module public TextDocument =

    let private lineStarts body =
        body
        |> Seq.indexed
        |> Seq.choose (fun (idx, ch) -> if ch = '\n' then Some(idx) else None)
        |> List.ofSeq

    /// Create a document with a synthetic (no source) ID.
    /// Suitable for tests and compiler-generated syntax.
    let public fromParts path body =
        { Path = path
          LineStarts = lineStarts body }

    /// Turn a character offset into a document into a human line column value.
    /// That is a 0-based character offset into the file becomes a 1-based pair
    /// of line and column.
    let private offsetToLineCol lines (offset: int) =
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

    /// Advance a text offset by a character count.
    ///
    /// Centralises the int → Firethorn.TextLength (uint32) widening conversion
    /// so that callers (the lexer, parser, …) can stay free of raw casts.
    let public advance (offset: Firethorn.TextLength) (count: int) : Firethorn.TextLength = offset + uint32 count

    let public offsetToPoint document (offset: Firethorn.TextLength) =
        let line, col = offsetToLineCol document.LineStarts (int offset)
        TextPoint.FromParts(document.Path, line, col)

    let public rangeToLocation document (range: Firethorn.TextRange) =
        let s = range.Start
        let e = range.End
        Span(s |> offsetToPoint document, e |> offsetToPoint document)
