module SpecTests

open System.Text.RegularExpressions

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
    |> Seq.map (fun x -> Path.GetRelativePath(specDir, x))

let executableSpecs = specsOfType "scm"
let librarySpecs = specsOfType "sld"

let private runExample host exePath =
    let p = new Process()
    p.StartInfo <- ProcessStartInfo(host)
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

let private parseDirectives (sourcePath: string) =
    let parseDirective (line: string) =
        let line = line.Trim()
        if line.StartsWith(';') then
            let m = Regex.Match(line, ";+\s*!(depends):(.+)")
            if m.Success then
                Some((m.Groups.[1].Value.ToLowerInvariant(), m.Groups.[2].Value))
            else
                None
        else
            None
    File.ReadAllLines(sourcePath)
    |> Seq.choose parseDirective


let public getRunTestData () =
    executableSpecs
    |> Seq.collect (fun spec ->
        [ [| spec :> obj; BuildConfiguration.Debug :> obj |]
        ; [| spec :> obj; BuildConfiguration.Release :> obj |] ])

[<Theory>]
[<MemberDataAttribute("getRunTestData")>]
let ``spec tests compile and run`` specPath configuration =

    let sourcePath = Path.Join(specDir, specPath)
    let options = CompilationOptions.Create configuration Exe
    let binDir = [| specBin; options.Configuration |> string |] |> Path.Combine

    let shouldFail = sourcePath.Contains "fail"
    let mutable references = []

    let artifactpath (options: CompilationOptions) source =
        Path.Join(binDir, Path.ChangeExtension(source, options.DefaultExtension))
    
    // Process directives from the source file. This is where we compile
    // dependant libraries.
    parseDirectives sourcePath
    |> Seq.iter (fun (directive, arg) ->
        match directive with
        | "depends" ->
            let libSourcePath = Path.Join(Path.GetDirectoryName(sourcePath), arg.Trim())
            let libOptions = { options with OutputType = Lib }
            let libPath = artifactpath libOptions (Path.GetFileName(libSourcePath))
            match compileFile libOptions libPath libSourcePath with
            | [] ->
                references <- List.append references [ libPath ]
            | diags ->
                failwithf "Compilation error in backing library: %A" diags
        | _ -> failwithf "unrecognised directive !%s: %s" directive arg)
    
    // Compile the output assembly, and run the appropriate assertions
    let options = { options with OutputType = Exe; References = references }
    let exePath = artifactpath options specPath
    match compileFile options exePath sourcePath with
    | [] ->
        if shouldFail then
            failwith "Expected compilation failure!"
        let r = runExample "dotnet" exePath
        r.ShouldMatchChildSnapshot(specPath)
    | diags ->
        if not shouldFail then
            failwithf "Compilation error: %A" diags
        (diags |> diagSanitiser).ShouldMatchChildSnapshot(specPath)

let public getParseTestData () =
    Seq.append librarySpecs executableSpecs 
    |> Seq.map (fun x -> [| x |])

[<Theory>]
[<MemberDataAttribute("getParseTestData")>]
let ``spec tests parse result`` s =
    let node, diagnostics = parseFile (Path.Join(specDir, s))
    let tree = (node |> nodeSanitiser, diagnostics |> diagSanitiser)
    tree.ShouldMatchSnapshot(Core.SnapshotId(snapDir, "Parse", s))

[<Fact>]
let testOne () =
    ``spec tests parse result`` "fail/bad-strings-and-idents.scm"
