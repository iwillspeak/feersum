module SpecTests

open System.Text.RegularExpressions

open Xunit
open Feersum.CompilerServices
open Feersum.CompilerServices.Compile
open Feersum.CompilerServices.Syntax
open System.IO
open Snapper
open System.Diagnostics
open SyntaxUtils
open System.Text
open System.Threading
open System
open System.Threading.Tasks

// This type has to be public so `Snapper` can see it.
type TestExecutionResult =
    { Output: string
      Error: string
      Exit: byte }

let specDir = Path.Join(__SOURCE_DIRECTORY__, "..", "..", "spec")

let specBin = Path.Join(specDir, "bin")

let snapDir = Path.Join(__SOURCE_DIRECTORY__, "_snapshots")

let normalisePath (path: string) = path.Replace("\\", "/")

let sanitiser =
    (basedLocation specDir)
    >> sanitiseLocationWith (sanitiseStreamNameWith normalisePath)

let nodeSanitiser = sanitiseNodeWith (sanitiser)

let diagSanitiser =
    sanitiseDiagnosticsWith (sanitiser)
    >> List.sortByDescending (fun x -> x.Location.Start)

let normaliseEndings (s: string) = s.Replace("\r\n", "\n")

let specsOfType extension =
    Directory.GetFiles(specDir, "*." + extension, SearchOption.AllDirectories)
    |> Seq.map (fun x -> Path.GetRelativePath(specDir, x))

let executableSpecs = specsOfType "scm"
let librarySpecs = specsOfType "sld"

let private runExampleAsync references host (exePath: string) =

    let p = new Process()
    p.StartInfo <- ProcessStartInfo(host)
    p.StartInfo.ArgumentList.Add(exePath)
    p.StartInfo.UseShellExecute <- false
    p.StartInfo.StandardErrorEncoding <- Encoding.UTF8
    p.StartInfo.StandardOutputEncoding <- Encoding.UTF8
    // TODO: support spec tests with `.input` files.
    // p.StartInfo.RedirectStandardInput <- true
    p.StartInfo.RedirectStandardOutput <- true
    p.StartInfo.RedirectStandardError <- true
    p.StartInfo.Environment[ "FEERSUM_TESTING" ] <- "test-sentinel"
    Assert.True(p.Start())

    task {
        let! output =
            Task.WhenAll(
                [| p.StandardOutput.ReadToEndAsync()
                   p.StandardError.ReadToEndAsync() |]
            )

        let! exit = p.WaitForExitAsync()

        return
            { Output = output[0] |> normaliseEndings
              Error = output[1] |> normaliseEndings
              // Exit codes on windows are full integers. Clamp everything to a byte so we
              // have partiy with POSIX systems
              Exit = byte ((uint p.ExitCode) &&& 0xFFu) }
    }

let private parseDirectives (sourcePath: string) =
    let parseDirective (line: string) =
        let line = line.Trim()

        if line.StartsWith(';') then
            let m = Regex.Match(line, ";+\s*!(depends):(.+)")

            if m.Success then
                Some((m.Groups[ 1 ].Value.ToLowerInvariant(), m.Groups[2].Value))
            else
                None
        else
            None

    task {
        let! lines = File.ReadAllLinesAsync(sourcePath)
        return lines |> Seq.choose parseDirective
    }

let public getRunTestData () =
    executableSpecs
    |> Seq.collect (fun spec ->
        [ [| spec :> obj
             BuildConfiguration.Debug :> obj |]
          [| spec :> obj
             BuildConfiguration.Release :> obj |] ])

[<Theory>]
[<MemberDataAttribute("getRunTestData")>]
let rec ``spec tests compile and run`` specPath configuration =
    let sourcePath = Path.Join(specDir, specPath)

    let options =
        { (CompilationOptions.Create configuration Exe) with GenerateDepsFiles = true }

    let binDir =
        [| specBin
           options.Configuration |> string |]
        |> Path.Combine

    let shouldFail = sourcePath.Contains "fail"

    let mutable references = [ typeof<Feersum.Core.LispProgram>.Assembly.Location ]

    let artifactpath (options: CompilationOptions) source =
        Path.Join(binDir, Path.ChangeExtension(source, options.DefaultExtension))

    task {
        // Process directives from the source file. This is where we compile
        // dependant libraries.
        let! directives = parseDirectives sourcePath

        directives
        |> Seq.iter (fun (directive, arg) ->
            match directive with
            | "depends" ->
                let libSourcePath = Path.Join(Path.GetDirectoryName(sourcePath), arg.Trim())

                let libOptions =
                    { options with
                        OutputType = Lib
                        References = references }

                let libPath = artifactpath libOptions (Path.GetFileName(libSourcePath))

                match Compilation.compileFile libOptions libPath libSourcePath with
                | [] -> references <- List.append references [ libPath ]
                | diags -> failwithf "Compilation error in backing library: %A" diags
            | _ -> failwithf "unrecognised directive !%s: %s" directive arg)

        // Compile the output assembly, and run the appropriate assertions
        let options =
            { options with
                OutputType = Exe
                References = references }

        let exePath = artifactpath options specPath
        let specName = specPath |> normalisePath

        let snapshotId =
            Core.SnapshotId(snapDir, "SpecTests", nameof (``spec tests compile and run``), specName, false)

        match Compilation.compileFile options exePath sourcePath with
        | [] ->
            if shouldFail then
                failwith "Expected compilation failure!"

            let! r = runExampleAsync references "dotnet" exePath

            r.ShouldMatchSnapshot(snapshotId)
        | diags ->
            if not shouldFail then
                failwithf "Compilation error: %A" diags

            (diags |> diagSanitiser)
                .ShouldMatchSnapshot(snapshotId)
    }

let public getParseTestData () =
    Seq.append librarySpecs executableSpecs
    |> Seq.map (fun x -> [| x |])

[<Theory>]
[<MemberDataAttribute("getParseTestData")>]
let ``spec tests parse result`` s =
    let node, diagnostics = Parse.parseFile (Path.Join(specDir, s))

    let tree = (node |> nodeSanitiser, diagnostics |> diagSanitiser)

    tree.ShouldMatchSnapshot(Core.SnapshotId(snapDir, "Parse", s |> normalisePath))

[<Theory>]
[<MemberDataAttribute("getParseTestData")>]
let ``Test new lexer`` s =
    task {
        let! sourceText = File.ReadAllTextAsync(Path.Join(specDir, s))
        let lexer = Lexer(sourceText, "test.scm")

        use timeout = new CancellationTokenSource(TimeSpan.FromSeconds(2.0))

        let mutable errors = 0
        let expectFail = s.Contains("bad")

        while not lexer.Done do
            timeout.Token.ThrowIfCancellationRequested()
            let (kind, token) = lexer.Current

            if kind = TokenKind.Error then
                if not expectFail then
                    printfn "Unexpected error token %s" token

                errors <- errors + 1

            lexer.Bump()

        // We expect error tokens in the lexer fail cases.
        if not expectFail then
            Assert.Equal(0, errors)
    }
