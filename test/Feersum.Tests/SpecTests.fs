module SpecTests

open Xunit
open Options
open Compile
open Syntax
open System.IO
open Snapper
open Snapper.Attributes
open System.Diagnostics
open SyntaxUtils

type TestExecutionResult =
    { Output: string
    ; Error: string
    ; Exit: int }

// [<assembly: UpdateSnapshots>]
// ()

let specDir = Path.Join(__SOURCE_DIRECTORY__, "..", "..", "spec")
let specBin = Path.Join(specDir, "bin")
let snapDir = Path.Join(__SOURCE_DIRECTORY__, "_snapshots")

let listSpecs =
    Directory.GetFiles(specDir, "*.scm", SearchOption.AllDirectories)
    |> Seq.map (fun x -> [| Path.GetRelativePath(specDir, x) |])

let private runExample exePath =
    let p = new Process()
    p.StartInfo <- ProcessStartInfo("dotnet")
    p.StartInfo.ArgumentList.Add(exePath)
    p.StartInfo.UseShellExecute <- false
    // p.StartInfo.RedirectStandardInput <- true
    p.StartInfo.RedirectStandardOutput <- true
    p.StartInfo.RedirectStandardError <- true
    Assert.True(p.Start())
    p.WaitForExit()
    { Output = p.StandardOutput.ReadToEnd()
    ; Error = p.StandardError.ReadToEnd()
    ; Exit = p.ExitCode }

[<Theory>]
[<MemberDataAttribute("listSpecs")>]
let ``spec tests compile and run`` s =
    let sourcePath = Path.Join(specDir, s)
    let exePath = Path.ChangeExtension(Path.Join(specBin, s), "dll")
    let shouldFail = sourcePath.Contains "fail"
    match compileFile BuildConfiguration.Debug exePath sourcePath with
    | [] ->
        if shouldFail then
            failwith "Expected compilation failure!"
        let r = runExample exePath
        r.ShouldMatchChildSnapshot(s)
    | diags ->
        if not shouldFail then
            failwithf "Compilation error: %A" diags
        (diags |> sanitiseDiagnostics specDir).ShouldMatchChildSnapshot(s)

[<Theory>]
[<MemberDataAttribute("listSpecs")>]
let ``spec tests parse result`` s =
    let node, diagnostics = parseFile (Path.Join(specDir, s))
    let tree = (node |> sanitiseWithPosition, diagnostics |> sanitiseDiagnostics specDir)
    tree.ShouldMatchSnapshot(Core.SnapshotId(snapDir, "Parse", s))

[<Fact>]
let testOne () =
    ``spec tests parse result`` "fail/bad-strings-and-idents.scm"
