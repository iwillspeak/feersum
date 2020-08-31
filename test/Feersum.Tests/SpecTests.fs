module SpecTests

open Xunit
open Compile
open Syntax
open System.IO
open Snapper
open Snapper.Attributes
open System.Diagnostics

// [<assembly: UpdateSnapshots>]
// ()

let specDir = Path.Join(__SOURCE_DIRECTORY__, "..", "..", "spec")
let specBin = Path.Join(specDir, "bin")
let snapDir = Path.Join(__SOURCE_DIRECTORY__, "_snapshots")

let listSpecs =
    Directory.GetFiles(specDir, "*.scm", SearchOption.AllDirectories)
    |> Seq.map (fun x -> [| Path.GetRelativePath(specDir, x) |])

[<Theory>]
[<MemberDataAttribute("listSpecs")>]
let ``spec tests compile and run`` s =
    let sourcePath = Path.Join(specDir, s)
    let exePath = Path.ChangeExtension(Path.Join(specBin, s), "exe")
    match compileFile exePath sourcePath with
    | Ok _ ->
        let p = Process.Start("dotnet", exePath)
        p.WaitForExit()
        p.ExitCode.ShouldMatchChildSnapshot(s)
    | Error e -> failwithf "Compilation error: %A" e

[<Theory>]
[<MemberDataAttribute("listSpecs")>]
let ``spect tests parse result`` s =
    let tree = parseFile (Path.Join(specDir, s))
    tree.ShouldMatchSnapshot(Core.SnapshotId(snapDir, "Parse", s))
