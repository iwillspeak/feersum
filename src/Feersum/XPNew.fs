module Feersum.XPNew

open Feersum.CompilerServices.Text
open Feersum.CompilerServices.Syntax
open Feersum.CompilerServices.NewBindingTest
open System.IO
open Feersum.CompilerServices.Syntax.Tree
open Feersum.CompilerServices.Syntax.Parse
open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Compile
open Feersum.CompilerServices.Targets
open Feersum.CompilerServices
open Feersum.CompilerServices.Binding

let runXpNewRepl (options: CompilationOptions) args =
    let registry = SourceRegistry.empty ()

    let target =
        match options.FrameworkAssmPaths with
        | [] -> TargetResolve.fromCurrentRuntime
        | paths -> TargetResolve.fromFrameworkPaths paths

    let (_, allLibs) =
        options.References
        |> Seq.map Builtins.loadReferencedSignatures
        |> Seq.append (Seq.singleton <| Builtins.loadCoreSignatures target)
        |> Seq.fold
            (fun (tys, sigs) (aTys, aSigs) -> (List.append tys aTys, List.append sigs aSigs))
            ([], [])

    let progs =
        List.map (fun path -> Parse.readProgram registry path (File.ReadAllText path)) args

    progs
    |> Seq.iter (fun prog ->
        if ParseResult.hasErrors prog then
            dumpDiagnostics prog.Diagnostics
        else
            let ctx = ExpandCtx.createGlobal registry "LispProgram" allLibs
            let initialScope =
                Builtins.loadBuiltinMacroEnv ()
                |> List.fold (fun s (name, tr) -> ExpandCtx.addMacro ctx name tr s) StxEnvironment.builtin
            let result = Expand.expandProgram prog.Root initialScope Map.empty ctx
            dumpDiagnostics ctx.Diagnostics.Diagnostics
            if not (hasErrors ctx.Diagnostics.Diagnostics) then
                printfn "%A" result)

let runXpOldRepl (options: CompilationOptions) args =
    let registry = SourceRegistry.empty ()

    let target =
        match options.FrameworkAssmPaths with
        | [] -> TargetResolve.fromCurrentRuntime
        | paths -> TargetResolve.fromFrameworkPaths paths

    let (_, allLibs) =
        options.References
        |> Seq.map Builtins.loadReferencedSignatures
        |> Seq.append (Seq.singleton <| Builtins.loadCoreSignatures target)
        |> Seq.fold
            (fun (tys, sigs) (aTys, aSigs) -> (List.append tys aTys, List.append sigs aSigs))
            ([], [])

    let scope = Binder.scopeFromLibraries allLibs

    // Strip SequencePoint wrappers so the output matches xpnew's format.
    let rec strip expr =
        match expr with
        | BoundExpr.SequencePoint(inner, _) -> strip inner
        | BoundExpr.Seq exprs -> BoundExpr.Seq(List.map strip exprs)
        | BoundExpr.Application(f, args) -> BoundExpr.Application(strip f, List.map strip args)
        | BoundExpr.If(c, t, e) -> BoundExpr.If(strip c, strip t, Option.map strip e)
        | BoundExpr.Store(s, v) -> BoundExpr.Store(s, Option.map strip v)
        | BoundExpr.Lambda(formals, body) ->
            BoundExpr.Lambda(formals, { body with Body = strip body.Body })
        | BoundExpr.Library(name, mn, exports, body) ->
            BoundExpr.Library(name, mn, exports, { body with Body = strip body.Body })
        | other -> other

    let progs =
        List.map (fun path -> Parse.readProgram registry path (File.ReadAllText path)) args

    progs
    |> Seq.iter (fun prog ->
        if ParseResult.hasErrors prog then
            dumpDiagnostics prog.Diagnostics
        else
            let units = [ prog.Root.Body |> List.ofSeq ]
            let result = Binder.bind scope allLibs registry units
            dumpDiagnostics result.Diagnostics
            if not (hasErrors result.Diagnostics) then
                match result.Root.Body with
                | BoundExpr.Seq stmts -> printfn "%A" (List.map strip stmts)
                | x -> printfn "%A" [ strip x ])
