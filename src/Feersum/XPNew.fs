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
        |> Seq.fold (fun (tys, sigs) (aTys, aSigs) -> (List.append tys aTys, List.append sigs aSigs)) ([], [])

    let progs =
        List.map (fun path -> Parse.readProgram registry path (File.ReadAllText path)) args

    progs
    |> Seq.iter (fun prog ->
        if ParseResult.hasErrors prog then
            dumpDiagnostics prog.Diagnostics
        else
            let ctx = ExpandCtx.createGlobal registry "LispProgram" allLibs
            let initialScope = Builtins.loadBuiltinMacroEnv ctx
            let result = Expand.expand [ prog.Root ] initialScope Map.empty ctx
            dumpDiagnostics result.Diagnostics

            if not (hasErrors result.Diagnostics) then
                match result.Root.Body with
                | BoundExpr.Seq stmts -> printfn "%A" stmts
                | x -> printfn "%A" [ x ])

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
        |> Seq.fold (fun (tys, sigs) (aTys, aSigs) -> (List.append tys aTys, List.append sigs aSigs)) ([], [])

    let scope = Binder.scopeFromLibraries allLibs

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
                | BoundExpr.Seq stmts -> printfn "%A" stmts
                | x -> printfn "%A" [ x ])
