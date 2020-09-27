module SpecTests

open Xunit
open Compile
open Syntax
open System.IO
open Snapper
open Snapper.Attributes
open System.Diagnostics
open SyntaxUtils

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
    let shouldFail = sourcePath.Contains "fail"
    match compileFile exePath sourcePath with
    | [] ->
        if shouldFail then
            failwith "Expected compilation failure!"
        let p = Process.Start("dotnet", exePath)
        p.WaitForExit()
        p.ExitCode.ShouldMatchChildSnapshot(s)
    | diags ->
        if not shouldFail then
            failwithf "Compilation error: %A" diags
        diags.ShouldMatchChildSnapshot(s)

[<Theory>]
[<MemberDataAttribute("listSpecs")>]
let ``spect tests parse result`` s =
    let node, diagnostics = parseFile (Path.Join(specDir, s))
    let tree = (node |> sanitise, diagnostics)
    tree.ShouldMatchSnapshot(Core.SnapshotId(snapDir, "Parse", s))
