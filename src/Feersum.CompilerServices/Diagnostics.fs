namespace Feersum.CompilerServices.Diagnostics

open System.IO

/// A point in the source text
type TextPoint =
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
type TextLocation =
    | Span of TextPoint * TextPoint
    | Point of TextPoint
    | Missing

    /// Get the start of the text location. This returns a cursor that lies just
    /// before any text represented by this locaiton.
    member x.Start =
        match x with
        | Span (s, _) -> s
        | Point p -> p
        | Missing -> TextPoint.FromParts("missing", 0, 0)

    /// Get the end of the text location. This returns a cursot that lies just
    /// after any text represented by this location.
    member x.End =
        match x with
        | Span (_, e) -> e
        | Point p -> p
        | Missing -> TextPoint.FromParts("missing", 0, 0)

/// Level of diagnostic. Used to tell warnings from errors.
type DiagnosticLevel =
    /// The diagnostic represents an error. Compilation should not continue.
    | Error
    /// The diagnostic represents a warning. Compilation may continue but the
    /// result may be unexpected or invalid.
    | Warning

/// Kind for a diagnostic. This holds all the information for a diagnostic that
/// is not related to the specific instance of that diagnostic. That is the
/// level, code, etc.
type DiagnosticKind =
    { Level: DiagnosticLevel
      Code: int
      Title: string }

    static member Create level code title =
        { Level = level
          Code = code
          Title = title }

/// Diagnostics indicate problems with our source code at a given position.
[<RequireQualifiedAccess>]
type Diagnostic =
    { Kind: DiagnosticKind
      Location: TextLocation
      Message: string }

    /// Create a new error diagnostic at the given `location`.`
    static member Create kind location message =
        { Kind = kind
          Location = location
          Message = message }

    /// Format the diagnostic for output.
    ///
    /// The output format shold conform to the [MSBuild "Canonical Error
    /// Format"](https://github.com/dotnet/msbuild/blob/94c28cca4cdb22f2cac279e3fd8d86aa4d061848/src/Shared/CanonicalError.cs#L12-L51)
    override d.ToString() =
        let normaliseName (stream: string) =
            if Path.IsPathRooted(stream) then
                stream
            else
                Path.Join(Directory.GetCurrentDirectory(), stream)

        match d.Location with
        | Missing -> sprintf "feersum: %s: %s" d.MessagePrefix d.FormattedMessage
        | Point p ->
            sprintf "%s(%d,%d): %s: %s" (p.Source |> normaliseName) p.Line p.Col d.MessagePrefix d.FormattedMessage
        | Span (s, e) ->
            // If both points are on the same line then we can use the a more
            // compact format.
            if s.Line = e.Line then
                sprintf
                    "%s(%d,%d-%d): %s: %s"
                    (s.Source |> normaliseName)
                    s.Line
                    s.Col
                    e.Col
                    d.MessagePrefix
                    d.FormattedMessage
            else 
                sprintf
                    "%s(%d,%d,%d,%d): %s: %s"
                    (s.Source |> normaliseName)
                    s.Line
                    s.Col
                    e.Line
                    e.Col
                    d.MessagePrefix
                    d.FormattedMessage

    /// Formatted value for the message
    member private d.FormattedMessage = sprintf "[%s] %s" d.Kind.Title d.Message

    /// Prefix for the message. Used to summarise the diagnostic kind.
    member private d.MessagePrefix =
        match d.Kind.Level with
        | Warning -> sprintf "warning SCM%04u" d.Kind.Code
        | Error -> sprintf "error SCM%04u" d.Kind.Code

/// A collection of diagnostics being built by a compiler phase.
type DiagnosticBag =
    { mutable Diagnostics: Diagnostic list }

    /// Buffer a diagnostic into the bag.
    member b.Emit kind pos message =
        Diagnostic.Create kind pos message |> b.Add

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
        | { Diagnostic.Kind = { Level = Error } } -> true
        | _ -> false

    /// Test if a given diagnostics collection contains any errors.
    let public hasErrors (diagnostics: Diagnostic seq) : bool = Seq.exists (isError) diagnostics

    /// Write the diagnostics to the standard error
    let dumpDiagnostics diags =
        diags
        |> List.rev
        |> Seq.iter (fun x -> eprintfn "%s" (x.ToString()))
