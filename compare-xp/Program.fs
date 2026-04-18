// compare-xp.fsx
//
// Compares Expand.expand with Binder.bind on the spec suite.
// Parses every spec/**/*.scm and *.sld directly using Feersum.CompilerServices,
// calls both functions, and compares their outputs and diagnostics.
//
// Usage:  dotnet fsi compare-xp.fsx [--no-build] [filter]
//   --no-build   skip the dotnet build step
//   filter       only run files whose path contains this string

open System
open System.IO
open System.Diagnostics

open Feersum.CompilerServices.Syntax.Parse
open Feersum.CompilerServices.Binding
open Feersum.CompilerServices.Text
open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Syntax.Tree
open Feersum.CompilerServices.Syntax
open Feersum.CompilerServices.Binding.New
open Feersum.CompilerServices.Compile
open Feersum.CompilerServices.Targets

let repoRoot = __SOURCE_DIRECTORY__

let args = Environment.GetCommandLineArgs() |> List.ofArray |> List.tail
let filter = args |> List.tryFind (fun a -> a <> "--no-build")

// ── Step 2: Discover spec files ───────────────────────────────────────────────

let specDir = Path.Combine(repoRoot, "../spec")

let specFiles =
    [| yield! Directory.GetFiles(specDir, "*.scm", SearchOption.AllDirectories)
       yield! Directory.GetFiles(specDir, "*.sld", SearchOption.AllDirectories) |]
    |> Array.sort
    |> Array.filter (fun f ->
        match filter with
        | None -> true
        | Some pat -> f.Contains(pat))

// ── Helper functions ──────────────────────────────────────────────────────────

let formatDiagnostics (diags: Diagnostic list) : string =
    diags |> List.map (fun d -> sprintf "  %s" (d.ToString())) |> String.concat "\n"

let tryParseFile (registry: SourceRegistry) (filePath: string) : Result<Tree.Program list, string> =
    try
        let source = File.ReadAllText(filePath)
        let result = readProgram registry (Path.GetFileName(filePath)) source
        Ok [ result.Root ]
    with ex ->
        Result.Error(sprintf "Parse error: %s" ex.Message)

let tryExpand (registry: SourceRegistry) (programs: Tree.Program list) libs : Result<BoundSyntaxTree, string> =
    try
        let ctx = ExpandCtx.createGlobal registry "LispProgram" libs
        let result = Expand.expand programs Map.empty Map.empty ctx
        Ok result
    with ex ->
        Result.Error(sprintf "Expand error: %s" ex.Message)

let tryBind (registry: SourceRegistry) (programs: Tree.Program list) libs : Result<BoundSyntaxTree, string> =
    try
        let result = Binder.bindProgram registry Map.empty Map.empty libs programs
        Ok result
    with ex ->
        Result.Error(sprintf "Bind error: %s" ex.Message)

let rec sanitiseTree =
    function
    // Old expander binds the args, new one short circuits.
    | BoundExpr.Application(BoundExpr.Error, _) -> BoundExpr.Error
    | BoundExpr.Application(op, args) -> BoundExpr.Application(sanitiseTree op, List.map (sanitiseTree) args)
    | BoundExpr.SequencePoint(inner, sp) -> BoundExpr.SequencePoint(sanitiseTree inner, sp)
    | BoundExpr.Store(dest, expr) -> BoundExpr.Store(dest, expr |> Option.map sanitiseTree)
    | BoundExpr.If(cond, t, f) -> BoundExpr.If(sanitiseTree cond, sanitiseTree t, f |> Option.map sanitiseTree)
    | BoundExpr.Seq(exprs) ->
        exprs
        |> List.collect (fun x ->
            match sanitiseTree x with
            | BoundExpr.Nop
            | BoundExpr.Seq [] -> []
            | BoundExpr.Seq inner -> inner
            | other -> [ other ])
        |> BoundExpr.Seq

    | BoundExpr.Lambda(_, _)
    | BoundExpr.Library(_, _, _, _)

    | BoundExpr.Load(_)
    | BoundExpr.Nop
    | BoundExpr.Error
    | BoundExpr.Literal(_)
    | BoundExpr.Quoted(_) as x -> x

let sanitise (body: BoundBody) =
    { body with
        Body = sanitiseTree body.Body }

let boundTreeToString (bst: BoundSyntaxTree) : string =
    sprintf
        "BoundSyntaxTree { MangledName = %s }\n%s\n%A"
        bst.MangledName
        (formatDiagnostics bst.Diagnostics)
        (bst.Root |> sanitise)

// ── Step 3: Compare ───────────────────────────────────────────────────────────

let mutable counts =
    Map.ofList [ "match", 0; "both-failed", 0; "differ", 0; "expand-fail", 0; "bind-fail", 0 ]

let inc key =
    counts <- counts |> Map.add key (counts.[key] + 1)

let truncate (n: int) (s: string) =
    if s.Length <= n then s else s.[.. n - 1] + "…"

let showDiff (label: string) (expandOut: string) (bindOut: string) =
    let tmp = Path.GetTempPath()
    let fexp = Path.Combine(tmp, "expand.txt")
    let fbind = Path.Combine(tmp, "bind.txt")
    File.WriteAllText(fexp, expandOut)
    File.WriteAllText(fbind, bindOut)
    let psi = ProcessStartInfo()
    psi.FileName <- "sh"
    psi.ArgumentList.Add("-c")

    psi.ArgumentList.Add(
        sprintf
            "diff -u --label expand --label bind '%s' '%s' | bat --language=diff --style=plain --paging=never"
            fexp
            fbind
    )

    psi.WorkingDirectory <- repoRoot
    psi.UseShellExecute <- false
    use p = Process.Start(psi)
    p.WaitForExit()

printfn "Comparing %d spec file(s)...\n" specFiles.Length

let target = TargetResolve.fromCurrentRuntime
let coreLibs = Builtins.loadCoreSignatures target |> snd

let frameworkLibs =
    typeof<Feersum.Core.LispProgram>.Assembly.Location
    |> Builtins.loadReferencedSignatures
    |> snd

let libs = coreLibs @ frameworkLibs

for spec in specFiles do
    let rel = Path.GetRelativePath(repoRoot, spec)
    let registry = SourceRegistry.empty ()

    match tryParseFile registry spec with
    | Result.Error err ->
        printfn "  ✗  %s  [parse error]" rel
        printfn "     %s" (truncate 120 err)
        inc "both-failed"
    | Result.Ok programs ->
        let expandRes = tryExpand registry programs libs
        let bindRes = tryBind registry programs libs

        match expandRes, bindRes with
        | Result.Error expandErr, Result.Error bindErr ->
            inc "both-failed"
            printfn "  -  %s  (both report errors)" rel
        | Result.Error expandErr, Result.Ok _ ->
            inc "expand-fail"
            printfn "  ✗  %s  [REGRESSION: Expand errors, Bind OK]" rel
            printfn "     %s" (truncate 120 (expandErr.Split('\n').[0]))
        | Result.Ok _, Result.Error bindErr ->
            inc "bind-fail"
            printfn "  ↑  %s  [Bind errors, Expand OK]" rel
            printfn "     %s" (truncate 120 (bindErr.Split('\n').[0]))
        | Result.Ok expandBst, Result.Ok bindBst ->
            let expandStr = boundTreeToString expandBst
            let bindStr = boundTreeToString bindBst

            if expandStr = bindStr then
                inc "match"
                printfn "  ✓  %s" rel
            else
                inc "differ"
                printfn "  ≠  %s  [output differs]" rel
                showDiff rel expandStr bindStr

// ── Summary ───────────────────────────────────────────────────────────────────

printfn ""
printfn "─────────────────────────────────────────────"
printfn "  ✓  matched:       %d" counts.["match"]
printfn "  ≠  differ:        %d" counts.["differ"]
printfn "  ✗  expand fails:  %d" counts.["expand-fail"]
printfn "  ↑  bind fails:    %d" counts.["bind-fail"]
printfn "  -  both-failed:   %d" counts.["both-failed"]
printfn "─────────────────────────────────────────────"
printfn "  total:            %d" specFiles.Length
