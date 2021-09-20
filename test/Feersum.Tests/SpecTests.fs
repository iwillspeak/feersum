module SpecTests

open Xunit
open Options
open Compile
open Syntax
open System.IO
open Snapper
open System.Diagnostics
open SyntaxUtils

// This type has to be public so `Snapper` can see it.
type TestExecutionResult =
    { Output: string
    ; Error: string
    ; Exit: int }

let specDir = Path.Join(__SOURCE_DIRECTORY__, "..", "..", "spec")
let specBin = Path.Join(specDir, "bin")
let snapDir = Path.Join(__SOURCE_DIRECTORY__, "_snapshots")

let nodeSanitiser = sanitiseNodeWith (basedLocation specDir)
let diagSanitiser =
    sanitiseDiagnosticsWith (basedLocation specDir)
    >> List.sortByDescending (fun x -> x.Location.Start)

let specsOfType extension =
    Directory.GetFiles(specDir, "*." + extension, SearchOption.AllDirectories)
    |> Seq.map (fun x -> [| Path.GetRelativePath(specDir, x) |])

let executableSpecs = specsOfType "scm"
let librarySpecs = specsOfType "sld"
let allSpecs = Seq.append executableSpecs librarySpecs

let private runExample exePath =
    let p = new Process()
    p.StartInfo <- ProcessStartInfo("dotnet")
    p.StartInfo.ArgumentList.Add(exePath)
    p.StartInfo.UseShellExecute <- false
    // TODO: support spec tests with `.input` files.
    // p.StartInfo.RedirectStandardInput <- true
    p.StartInfo.RedirectStandardOutput <- true
    p.StartInfo.RedirectStandardError <- true
    Assert.True(p.Start())
    let output =
        [ p.StandardOutput.ReadToEndAsync() |> Async.AwaitTask
        ; p.StandardError.ReadToEndAsync() |> Async.AwaitTask ]
        |> Async.Parallel
        |> Async.RunSynchronously
    p.WaitForExit()
    { Output = output.[0]
    ; Error = output.[1]
    ; Exit = p.ExitCode }

[<Theory>]
[<MemberDataAttribute("executableSpecs")>]
let ``spec tests compile and run`` s =
    let sourcePath = Path.Join(specDir, s)
    let exePath = Path.ChangeExtension(Path.Join(specBin, s), "dll")
    let shouldFail = sourcePath.Contains "fail"
    let libSourcePath = Path.ChangeExtension(sourcePath, "sld")
    let references =
        if File.Exists(libSourcePath) then
            let libPath = Path.Join(specBin, Path.GetFileNameWithoutExtension(libSourcePath) + "-backing.dll")
            let libOptions = CompilationOptions.Create BuildConfiguration.Debug Lib
            match compileFile libOptions libPath libSourcePath with
            | [] ->
                [ libPath ]
            | diags ->
                failwithf "Compilation error in backing library: %A" diags
        else
            []
    let options =
        CompilationOptions.Create BuildConfiguration.Debug Exe
        |> (fun x -> x.WithReferences references)
    match compileFile options exePath sourcePath with
    | [] ->
        if shouldFail then
            failwith "Expected compilation failure!"
        let r = runExample exePath
        r.ShouldMatchChildSnapshot(s)
    | diags ->
        if not shouldFail then
            failwithf "Compilation error: %A" diags
        (diags |> diagSanitiser).ShouldMatchChildSnapshot(s)

[<Theory>]
[<MemberDataAttribute("allSpecs")>]
let ``spec tests parse result`` s =
    let node, diagnostics = parseFile (Path.Join(specDir, s))
    let tree = (node |> nodeSanitiser, diagnostics |> diagSanitiser)
    tree.ShouldMatchSnapshot(Core.SnapshotId(snapDir, "Parse", s))

[<Fact>]
let testOne () =
    ``spec tests parse result`` "fail/bad-strings-and-idents.scm"
