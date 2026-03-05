module SyntaxUtils

open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Text
open System.IO

/// Transform the StreamName in a Position
///
/// Re-writes the path in a given `Position` with a given `reWriter`.
let public sanitiseStreamNameWith reWriter (p: TextPoint) =
    TextPoint.FromParts(p.Source |> reWriter, p.Line, p.Col)

/// Path Re-Writer that returns a fixed stream name
let public fixedStreamName name =
    sanitiseStreamNameWith (function
        | _ -> name)

/// Path Re-Writer that normalises paths relative to `base`
let public basedStreamName basePath =
    sanitiseStreamNameWith (function
        | path -> Path.GetRelativePath(basePath, path))

/// Transform a location with the given Path Re-Writer
let public sanitiseLocationWith rewriter =
    function
    | Point(p) -> Point(rewriter p)
    | Span(s, e) -> Span(rewriter s, rewriter e)
    | Missing -> Missing

/// Location Re-writer that uses the `fixedStreamName` Path Re-writer
let public fixedLocaiton path =
    sanitiseLocationWith (fixedStreamName path)

/// Location Re-writer that uses the `basedStreamName` Path Re-writer
let public basedLocation basePath =
    sanitiseLocationWith (basedStreamName basePath)

/// Transofrm a diagnostics list with the given Location Re-Writer `rewriter`
let public sanitiseDiagnosticsWith rewriter (diags: Diagnostic list) =
    let sanitiseDiag d =
        { d with
            Diagnostic.Location = rewriter (d.Location) }

    List.map (sanitiseDiag) diags
