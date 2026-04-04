/// compare-xp.fsx
///
/// Rebuilds the Feersum compiler, then runs every spec/**/*.scm and *.sld
/// through both `xpnew` and `xpold`, comparing their stdout.  Diagnostics
/// (errors) go to stderr in the compiler, so stdout is the pure bound-form
/// output and stderr is checked only for failure detection.
///
/// Usage:  dotnet fsi compare-xp.fsx [--no-build] [filter]
///   --no-build   skip the dotnet build step
///   filter       only run files whose path contains this string

open System
open System.Diagnostics
open System.IO

let repoRoot = __SOURCE_DIRECTORY__

let args = fsi.CommandLineArgs |> Array.toList |> List.tail // drop script name
let noBuild = args |> List.contains "--no-build"
let filter = args |> List.tryFind (fun a -> a <> "--no-build")

// ── Process helpers ───────────────────────────────────────────────────────────

let runProcess (exe: string) (extraArgs: string list) =
    let psi = ProcessStartInfo()
    psi.FileName <- exe

    for a in extraArgs do
        psi.ArgumentList.Add(a)

    psi.WorkingDirectory <- repoRoot
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.UseShellExecute <- false
    use p = Process.Start(psi)
    // Read both streams concurrently to avoid deadlocks on large output.
    let stdoutTask = p.StandardOutput.ReadToEndAsync()
    let stderrTask = p.StandardError.ReadToEndAsync()
    p.WaitForExit()
    stdoutTask.Result, stderrTask.Result, p.ExitCode

// ── Step 1: Build ─────────────────────────────────────────────────────────────

if not noBuild then
    printfn "Building..."

    let out, err, code =
        runProcess "dotnet" [ "build"; "--no-incremental"; "-v"; "q"; "--configuration"; "Release" ]

    if code <> 0 then
        eprintfn "Build failed:\n%s%s" out err
        Environment.Exit 1

    printfn "Build succeeded.\n"

// ── Step 2: Discover spec files ───────────────────────────────────────────────

let specDir = Path.Combine(repoRoot, "spec")

let specFiles =
    [| yield! Directory.GetFiles(specDir, "*.scm", SearchOption.AllDirectories)
       yield! Directory.GetFiles(specDir, "*.sld", SearchOption.AllDirectories) |]
    |> Array.sort
    |> Array.filter (fun f ->
        match filter with
        | None -> true
        | Some pat -> f.Contains(pat))

let dll =
    Path.Combine(repoRoot, "src", "Feersum", "bin", "Release", "net8.0", "Feersum.dll")

if not (File.Exists dll) then
    eprintfn "Compiler DLL not found: %s\nRun without --no-build first." dll
    Environment.Exit 1

let runMode (mode: string) (spec: string) =
    let stdout, stderr, _code = runProcess "dotnet" [ dll; mode; spec ]
    let hasError = stderr.Contains("error SCM") || stdout.Contains("error SCM")
    not hasError, stdout.Trim(), stderr.Trim()

// ── Step 3: Compare ───────────────────────────────────────────────────────────

type Result =
    | Match // both succeeded and output agrees
    | BothFailed // both reported errors
    | Differ of string * string // both succeeded but output differs
    | Regression of string // xpnew errors, xpold OK
    | Improvement of string // xpnew OK, xpold errors

let mutable counts =
    Map.ofList [ "match", 0; "both-failed", 0; "differ", 0; "regr", 0; "impr", 0 ]

let inc key =
    counts <- counts |> Map.add key (counts.[key] + 1)

let truncate (n: int) (s: string) =
    if s.Length <= n then s else s.[.. n - 1] + "…"

let showDiff (label: string) (newOut: string) (oldOut: string) =
    let tmp = Path.GetTempPath()
    let fnew = Path.Combine(tmp, "xpnew.txt")
    let fold = Path.Combine(tmp, "xpold.txt")
    File.WriteAllText(fnew, newOut)
    File.WriteAllText(fold, oldOut)
    // Pass the whole pipeline to sh -c as a single argument via ArgumentList.
    let psi = ProcessStartInfo()
    psi.FileName <- "sh"
    psi.ArgumentList.Add("-c")

    psi.ArgumentList.Add(
        sprintf
            "diff -u --label xpold --label xpnew '%s' '%s' | bat --language=diff --style=plain --paging=never"
            fold
            fnew
    )

    psi.WorkingDirectory <- repoRoot
    psi.UseShellExecute <- false
    use p = Process.Start(psi)
    p.WaitForExit()

printfn "Comparing %d spec file(s)...\n" specFiles.Length

for spec in specFiles do
    let rel = Path.GetRelativePath(repoRoot, spec)
    let newOk, newOut, _newErr = runMode "xpnew" spec
    let oldOk, oldOut, _oldErr = runMode "xpold" spec

    let result =
        match newOk, oldOk with
        | false, false -> BothFailed
        | false, true -> Regression _newErr
        | true, false -> Improvement _oldErr
        | true, true -> if newOut = oldOut then Match else Differ(newOut, oldOut)

    match result with
    | Match ->
        inc "match"
        printfn "  ✓  %s" rel
    | BothFailed ->
        inc "both-failed"
        printfn "  -  %s  (both report errors)" rel
    | Regression msg ->
        inc "regr"
        printfn "  ✗  %s  [REGRESSION: xpnew errors, xpold OK]" rel
        printfn "     %s" (truncate 120 (msg.Split('\n').[0]))
    | Improvement msg ->
        inc "impr"
        printfn "  ↑  %s  [xpnew OK, xpold errors]" rel
        printfn "     %s" (truncate 120 (msg.Split('\n').[0]))
    | Differ(n, o) ->
        inc "differ"
        printfn "  ≠  %s  [output differs]" rel
        showDiff rel n o

// ── Summary ───────────────────────────────────────────────────────────────────

printfn ""
printfn "─────────────────────────────────────────────"
printfn "  ✓  matched:      %d" counts.["match"]
printfn "  ≠  differ:       %d" counts.["differ"]
printfn "  ✗  regressions:  %d" counts.["regr"]
printfn "  ↑  improvements: %d" counts.["impr"]
printfn "  -  both-failed:  %d" counts.["both-failed"]
printfn "─────────────────────────────────────────────"
printfn "  total:           %d" specFiles.Length
