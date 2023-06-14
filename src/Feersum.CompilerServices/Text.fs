namespace Feersum.CompilerServices.Text

/// A point in the source text
type public TextPoint =
    // FIXME: this _should_ just be the offset into the file, with line and
    // other information resolved later from a workspace or similar. We're stuck
    // like this for the time being though becuase of FParsec.
    { Source: string
      Line: int64
      Col: int64 }

    static member public FromExternal(position: FParsec.Position) : TextPoint =
        TextPoint.FromParts(position.StreamName, position.Line, position.Column)

    static member public FromParts(source: string, line: int64, col: int64) =
        { Source = source
          Line = line
          Col = col }

/// A lcation in the source text
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

    let private offsetToLineCol lines offset =
        match List.tryFindIndex (fun x -> x > offset) lines with
        | Some(0) -> (1, offset)
        | Some(idx) -> (idx, offset - lines[idx - 1])
        | None ->
            let lineCount = List.length lines

            if lineCount = 0 then
                (1, offset)
            else
                (lineCount, offset - (List.last lines))

    let public offsetToPoint document offset =
        let (line, col) = offsetToLineCol document.LineStarts offset
        TextPoint.FromParts(document.Path, line, col)

    let public rangeToLocation document (range: Firethorn.TextRange) =
        let s = range.Start
        let e = range.End
        Span(s |> offsetToPoint document, e |> offsetToPoint document)
