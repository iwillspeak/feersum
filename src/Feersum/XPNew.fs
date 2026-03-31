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
            let ctx = ExpandCtx.createGlobal registry "xpnew" allLibs
            let initialEnv =
                Builtins.loadBuiltinMacroEnv ()
                |> Map.fold (fun e k v -> Map.add k v e) SyntaxEnv.builtin
            let result = Expand.expandProgram prog.Root initialEnv ctx
            dumpDiagnostics ctx.Diagnostics.Diagnostics
            if not (hasErrors ctx.Diagnostics.Diagnostics) then
                printfn "%A" result)
