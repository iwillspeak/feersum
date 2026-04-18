module BinderTests

open Xunit
open Feersum.CompilerServices.Binding.New
open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Syntax
open Feersum.CompilerServices.Syntax.Parse
open Feersum.CompilerServices.Text
open Feersum.CompilerServices.Compile
open Feersum.CompilerServices.Targets
open Feersum.CompilerServices.Binding

// -- Helpers ------------------------------------------------------------------

let private registry = SourceRegistry.empty ()

/// Parse a Scheme program string and run the new expander.
/// Returns (BoundExpr list, Diagnostics).
let private expand (source: string) =
    let prog = Parse.readProgram registry "test" source

    if ParseResult.hasErrors prog then
        failwithf "Parse error in '%s': %A" source prog.Diagnostics

    let coreLibs = Builtins.loadCoreSignatures TargetResolve.fromCurrentRuntime |> snd

    let result =
        Binder.bindProgram registry Environments.emptyStx Map.empty coreLibs Map.empty [ prog.Root ]

    match result.Root.Body with
    | BoundExpr.Seq stmts -> stmts, result.Diagnostics
    | x -> [ x ], result.Diagnostics

/// Expand and assert there are no errors; return the bound expressions.
let private expandOk (source: string) =
    let exprs, diags = expand source

    let errors = diags |> List.filter (fun d -> d.Kind.Level = DiagnosticLevel.Error)

    if not (List.isEmpty errors) then
        let msgs = errors |> List.map (fun d -> d.Message) |> String.concat "; "
        failwithf "Unexpected errors in '%s': %s" source msgs

    exprs

// -- Binder tests -------------------------------------------------------------

[<Fact>]
let ``binder: bind single import`` () =
    let exprs = expandOk "(import (scheme write))"

    Assert.Empty exprs
