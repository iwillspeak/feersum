namespace Feersum.CompilerServices.Binding

open Feersum.CompilerServices.Syntax
open Feersum.CompilerServices.Syntax.Tree
open Feersum.CompilerServices.Text
open Feersum.CompilerServices.Diagnostics
open Firethorn
open Feersum.CompilerServices.Ice

/// Syntax Illuminiation Contex
///
/// The context used during syntax illumination. This contains the source document and
/// the diagnostics bag. This is used to report any errors encountered during
/// illumination, such as malformed literals.
type IlluminationCtx =
    { Doc: TextDocument
      Diagnostics: DiagnosticBag }

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

    let malformedDatum =
        DiagnosticKind.Create DiagnosticLevel.Error 102 "Malformed datum in syntax item"

    let incompleteDottedTail =
        DiagnosticKind.Create DiagnosticLevel.Error 103 "Incomplete dotted tail in syntax item"

/// Syntax Expansion
///
/// Syntax expansion is the process of transforming the `Stx` tree into a form
/// that is easier to bind. The main purpose of this stage is to resolve the
/// syntax of the source text so that no references to syntactic construcs other
/// than our well known `SpecialFormKind`s remain.
module Expand =

    let private makePos (ctx: IlluminationCtx) (range: TextRange) : StxPos =
        { StxPos.Doc = ctx.Doc
          StxPos.Range = range }

    /// Map a `Constant` from the CST into an `StxConstant` for the `Stx` tree.
    let private mapConstant (ctx: IlluminationCtx) (c: Constant) : StxConstant =
        let e =
            ctx.Diagnostics.Emit ExpanderDiagnostics.malformedDatum (TextDocument.rangeToLocation ctx.Doc c.SyntaxRange)

        match c.Value with
        | Some(BoolVal b) -> StxConstant.Boolean b
        | Some(CharVal ch) ->
            match ch with
            | Some ch -> StxConstant.Character ch
            | None ->
                e "Malformed character constant"
                StxConstant.Null
        | Some(NumVal n) -> StxConstant.Number n
        | Some(StrVal s) -> StxConstant.Str s
        | None ->
            e "Malformed constant"
            StxConstant.Null

    /// Project the elements of an `Expression` tree into an `Stx` tree.
    ///
    /// This is the first stage of syntax expansion. In this tree no syntax
    /// environment is active so all identifers are stamped with the
    /// `Ident.GlobalStamp` value.
    let rec illuminateExpr (ctx: IlluminationCtx) (expr: Expression) : Stx =
        match expr with
        | ByteVecNode b ->
            let bytes =
                b.Body
                |> Seq.choose (fun x ->
                    match x.Value with
                    | Ok v -> Some v
                    | Result.Error msg ->
                        ctx.Diagnostics.Emit
                            ExpanderDiagnostics.malformedDatum
                            (TextDocument.rangeToLocation ctx.Doc x.SyntaxRange)
                            msg

                        None)
                |> List.ofSeq

            let pos = makePos ctx expr.SyntaxRange
            Stx.Literal(StxLiteral.SelfEval(StxConstant.ByteVec bytes, pos), pos)
        | VecNode v ->
            let body = Seq.map ((illuminateExpr ctx) >> StxDatum.Compound) v.Body |> Seq.toList
            let pos = makePos ctx expr.SyntaxRange
            Stx.Literal(StxLiteral.SelfEval(StxConstant.Vector body, pos), pos)
        | FormNode f ->
            let tailStx =
                match f.DottedTail with
                | Some tail ->
                    match tail.Body with
                    | Some tailExpr -> illuminateExpr ctx tailExpr |> Some
                    | None ->
                        ctx.Diagnostics.Emit
                            ExpanderDiagnostics.incompleteDottedTail
                            (TextDocument.rangeToLocation ctx.Doc tail.SyntaxRange)
                            "Malformed dotted tail in form"

                        None
                | None -> None

            let bodyStx = Seq.map (illuminateExpr ctx) f.Body |> Seq.toList
            Stx.Form(bodyStx, tailStx, makePos ctx expr.SyntaxRange)
        | ConstantNode c ->
            Stx.Literal(StxLiteral.SelfEval(mapConstant ctx c, makePos ctx c.SyntaxRange), makePos ctx expr.SyntaxRange)
        | SymbolNode s -> Stx.Symbol(Ident.mint s.CookedValue, makePos ctx s.SyntaxRange)
        | QuotedNode q ->
            let innerStx =
                match q.Inner with
                | Some inner -> illuminateExpr ctx inner
                | None ->
                    ctx.Diagnostics.Emit
                        ExpanderDiagnostics.malformedDatum
                        (TextDocument.rangeToLocation ctx.Doc q.SyntaxRange)
                        "Malformed quoted expression"

                    Stx.Literal(
                        StxLiteral.SelfEval(StxConstant.Null, makePos ctx q.SyntaxRange),
                        makePos ctx q.SyntaxRange
                    )

            let pos = makePos ctx expr.SyntaxRange
            Stx.Literal(StxLiteral.Quotation(innerStx |> StxDatum.Compound, pos), pos)
(*
    let rec expand (ctx: StxCtx) (stx: Stx) : StxCtx * Stx option =

        let expandSimpleLst ctx body = body |> List.map (expandSimple ctx)

        let expandSimpleForm ctx body tail span =
            let expandedBody = expandSimpleLst ctx body
            let expandedTail = tail |> Option.map (expandSimple ctx)
            ctx, Stx.Form(expandedBody, expandedTail, span) |> Some

        match stx with
        // pass-through cases. These are items where the syntax can not
        // contain any further syntax items, so we can just return them as-is.
        | Stx.Form([], _, _) -> ctx, Some stx

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
        | Stx.Form(body, tail, span) ->
            match body with
            | [] -> ctx, Some stx
            | Stx.Symbol(id, idSpan) :: rest ->
                let resolved = StxEnv.resolve id ctx.Env

                match resolved with
                | StxBinding.Macro _ -> unimpl "Macro expansion not implemented yet"
                | StxBinding.SpecialForm specialForm -> expandSpecialForm specialForm ctx rest tail span
                | StxBinding.Var id ->
                    let body = (Stx.Symbol(id, idSpan) :: expandSimpleLst ctx rest)
                    let tail = tail |> Option.map (expandSimple ctx)
                    ctx, Stx.Form(body, tail, span) |> Some
            | _ -> expandSimpleForm ctx body tail span

    and expandSpecialForm (specialForm: SpecialFormKind) ctx body tail span =
        unimpl " Special form expansion not implemented yet"

    and expandSimple (ctx: StxCtx) (stx: Stx) : Stx =
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
 *)
