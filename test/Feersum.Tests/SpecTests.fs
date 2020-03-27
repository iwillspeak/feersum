module SpecTests

open Xunit
open Compile
open Syntax
open System.IO
open Snapper
open Snapper.Attributes

let specDir = Path.Join(__SOURCE_DIRECTORY__, "..", "..", "spec")

let listSpecs =
    Directory.GetFiles(specDir, "*.scm", SearchOption.AllDirectories)
    |> Seq.map (fun x -> [| Path.GetRelativePath(specDir, x) |])

[<Theory>]
[<MemberDataAttribute("listSpecs")>]
let ``spec tests compile`` s =
    match compileFile (Path.Join(specDir, s)) with
    | Ok _ -> ()
    | Error e -> failwithf "Compilation error: %A" e

[<Theory>]
[<MemberDataAttribute("listSpecs")>]
let ``spect tests parse result`` s =
    let tree = parseFile (Path.Join(specDir, s))
    tree.ShouldMatchSnapshot(Core.SnapshotId(specDir, "Parse", s))