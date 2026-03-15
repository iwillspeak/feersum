namespace Feersum.CompilerServices.Binding

open Feersum.CompilerServices.Syntax
open Feersum.CompilerServices.Syntax.Tree
open Feersum.CompilerServices.Text
open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Ice

/// Syntax Expansion Context
/// 
/// The context used during syntax expansion. This contains the source document and
/// the syntax environment. The syntax environment is used to resolve references to
/// syntax identifiers during expansion.
type StxCtx =
    { Doc: TextDocument
      Diagnostics: DiagnosticBag
      Env: StxEnv }

module private ExpanderDiagnostics =

    let invalidSyntaxItemUse =
        DiagnosticKind.Create DiagnosticLevel.Error 101 "Invalid syntax item use"

/// Syntax Expansion
/// 
/// Syntax expansion is the process of transforming the `Stx` tree into a form
/// that is easier to bind. The main purpose of this stage is to resolve the
/// syntax of the source text so that no references to syntactic construcs other
/// than our well known `SpecialFormKind`s remain.
module Expand =

    /// Project the elements of an `Expression` tree into an `Stx` tree.
    /// 
    /// This is the first stage of syntax expansion. In this tree no syntax
    /// environment is active so all identifers are stamped with the
    /// `Ident.GlobalStamp` value.
    let rec illuminateExpr (expr: Expression): Stx =
        match expr with
        | SymbolNode s -> Stx.Symbol (Ident.mint s.CookedValue, expr.SyntaxRange)
        | FormNode f ->
            let bodyStx = Seq.map illuminateExpr f.Body |> Seq.toList
            let tailStx =
                f.DottedTail
                // FIXME: This bind hides malformed or empty dotted tails
                |> Option.bind (fun x -> x.Body) 
                |> Option.map illuminateExpr
            Stx.Form (bodyStx, tailStx, expr.SyntaxRange)
        | _ -> Stx.Bypass expr

    let rec expand (ctx: StxCtx) (stx: Stx): StxCtx * Stx option =

        let expandSimpleLst ctx body =
            body |> List.map (expandSimple ctx)

        let expandSimpleForm ctx body tail span =
            let expandedBody = expandSimpleLst ctx body
            let expandedTail =
                tail
                |> Option.map (expandSimple ctx)
            ctx, Stx.Form(expandedBody, expandedTail, span) |> Some

        match stx with
        // pass-through cases. These are items where the syntax can not
        // contain any further syntax items, so we can just return them as-is.
        | Stx.Form ([] , _, _)
        | Stx.Bypass _ -> ctx, Some stx

        // Identifiers need to be resolved, to identify the case where a
        // syntax item is being used as an identifier, which is not valid. This
        // also applies any active alpha-renaming in the syntax context.
        | Stx.Symbol(id, span) ->
            let resolved = StxEnv.resolve id ctx.Env
            match resolved with            
            | Var rId -> (ctx, Stx.Symbol(rId, span) |> Some)
            | _ ->
                ctx.Diagnostics.Emit
                    ExpanderDiagnostics.invalidSyntaxItemUse
                    (TextDocument.rangeToLocation ctx.Doc span)
                    (sprintf "Identifeier %s refers to a syntax item in this context" id.Name)
                (ctx, None)

        // Closures expand in the inner environment, and return the resulting
        // syntax. This effecitvely strips the closure from teh tree.
        | Stx.Closure(stx, env) ->
            // TODO: We should also modify the text document here, once that
            //       is part of the closure.
            let innerCtx = { ctx with Env = env }
            let _, result = expand innerCtx stx
            ctx, result
        | Stx.Form (body, tail, span) ->
            match body with
            | [] -> ctx, Some stx
            | Stx.Symbol (id, idSpan) :: rest ->
                let resolved = StxEnv.resolve id ctx.Env
                match resolved with
                | StxBinding.Macro _ -> unimpl "Macro expansion not implemented yet"
                | StxBinding.SpecialForm specialForm ->
                    expandSpecialForm specialForm ctx rest tail span
                | StxBinding.Var id ->
                    let body = (Stx.Symbol(id, idSpan) :: expandSimpleLst ctx rest)
                    let tail = tail |> Option.map (expandSimple ctx)
                    ctx, Stx.Form(body, tail, span) |> Some
            | _ ->
                expandSimpleForm ctx body tail span

    and expandSpecialForm (specialForm: SpecialFormKind) ctx body tail span =
        unimpl " Special form expansion not implemented yet"

    and expandSimple (ctx: StxCtx) (stx: Stx): Stx =
        let ctx, expanded = expand ctx stx
        match expanded with
        | Some stx -> stx
        | None ->
            // This case is hit when we encounter an invalid use of a syntax item,
            // such as using a syntax item as an identifier. In this case we emit
            // a diagnostic and return the original syntax, which will likely
            // cause further errors during binding, but prevents us from emitting
            // a cascade of diagnostics.
            ctx.Diagnostics.Emit
                ExpanderDiagnostics.invalidSyntaxItemUse
                (TextDocument.rangeToLocation ctx.Doc (Stx.rangeOf stx))
                "Syntax expanded to empty item in an expression context"
            stx
