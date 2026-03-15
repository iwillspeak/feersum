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

    let private invalidSyntaxItemUse =
        DiagnosticKind.Create DiagnosticLevel.Error 104 "Invalid syntax item use"

    let private emitAt (ctx: StxCtx) kind (pos: StxPos) msg =
        ctx.Diagnostics.Emit kind (TextDocument.rangeToLocation pos.Doc pos.Range) msg

    /// Stub for macro transformer parsing. Not yet implemented.
    let private parseMacro (_name: Ident) (_transformer: Stx) : Transfomer =
        unimpl "Macro parsing is not yet implemented"

    /// Alpha-rename a single binding-position symbol in the env.
    /// Non-symbol nodes are passed through unchanged (ill-formed; the binder will report).
    let private renameFormal (env: StxEnv) (stx: Stx) : Stx * StxEnv =
        match stx with
        | Stx.Symbol(id, pos) ->
            let newId, env' = StxEnv.rename id env
            Stx.Symbol(newId, pos), env'
        | _ -> stx, env

    /// Alpha-rename a full formals pattern, covering the three R7RS lambda cases:
    ///   * `x`         — bare rest-arg symbol
    ///   * `(x y z)`   — fixed-arg list
    ///   * `(x y . z)` — fixed + rest (dotted list)
    /// Returns the rewritten formals node and the env extended with all new bindings.
    let private renameFormalsPattern (env: StxEnv) (formals: Stx) : Stx * StxEnv =
        match formals with
        | Stx.Symbol _ -> renameFormal env formals
        | Stx.Form(body, tail, pos) ->
            let renamedBody, env' = List.mapFold renameFormal env body
            let renamedTail, env'' =
                match tail with
                | Some t -> let t', e = renameFormal env' t in Some t', e
                | None -> None, env'
            Stx.Form(renamedBody, renamedTail, pos), env''
        | _ -> formals, env // ill-formed; pass through for the binder to diagnose

    /// Expand a `Stx` tree, resolving macro applications and `define-syntax` forms.
    ///
    /// Returns the updated `StxCtx` (with any new macro bindings added to `Env`) and
    /// an optional `Stx`. `None` means this form was consumed with no output
    /// (e.g. `define-syntax`).
    let rec expand (ctx: StxCtx) (stx: Stx) : StxCtx * Stx option =

        let expandSimpleLst ctx body = body |> List.map (expandSimple ctx)

        let expandSimpleForm ctx body tail pos =
            let expandedBody = expandSimpleLst ctx body
            let expandedTail = tail |> Option.map (expandSimple ctx)
            ctx, Stx.Form(expandedBody, expandedTail, pos) |> Some

        match stx with
        // Literals contain no syntax references — pass through unchanged.
        | Stx.Literal _ -> ctx, Some stx

        // Empty form is the null literal — pass through.
        | Stx.Form([], _, _) -> ctx, Some stx

        // Identifiers: resolve to detect bare use of syntax keywords as values.
        | Stx.Symbol(id, pos) ->
            match StxEnv.resolve id ctx.Env with
            | Var rId -> ctx, Stx.Symbol(rId, pos) |> Some
            | _ ->
                emitAt ctx invalidSyntaxItemUse pos
                    (sprintf "Identifier '%s' refers to a syntax item and cannot be used as a value" id.Name)

                ctx, None

        // Closures: expand the enclosed syntax in the captured environment, then
        // return the result to the outer context. The closure itself is stripped.
        | Stx.Closure(inner, env) ->
            let innerCtx = { ctx with Env = env }
            let _, result = expand innerCtx inner
            ctx, result

        // Forms: inspect the head symbol to decide how to proceed.
        | Stx.Form(body, tail, pos) ->
            match body with
            | Stx.Symbol(id, idPos) :: rest ->
                match StxEnv.resolve id ctx.Env with

                // `define-syntax`: parse the transformer, bind it in the env,
                // and produce no output syntax.
                | StxBinding.SpecialForm DefineSyntax ->
                    match rest with
                    | [ Stx.Symbol(nameId, _); transformerStx ] ->
                        let transformer = parseMacro nameId transformerStx
                        let newEnv = ctx.Env.Add(nameId, StxBinding.Macro transformer)
                        { ctx with Env = newEnv }, None
                    | _ ->
                        emitAt ctx invalidSyntaxItemUse pos "Ill-formed 'define-syntax'"
                        ctx, None

                // Macro application: apply the transformer to the whole form, then
                // re-expand the result so chained macros are handled.
                | StxBinding.Macro transformer ->
                    let expanded = transformer stx ctx.Env
                    expand ctx expanded

                // `lambda`: rename formals into a fresh inner env; the renamed
                // bindings do not escape back to the caller.
                | StxBinding.SpecialForm Lambda ->
                    let keyword = Stx.Symbol(id, idPos)
                    match rest with
                    | formals :: body ->
                        let renamedFormals, innerEnv = renameFormalsPattern ctx.Env formals
                        let innerCtx = { ctx with Env = innerEnv }
                        let expandedBody = expandSimpleLst innerCtx body
                        ctx, Stx.Form(keyword :: renamedFormals :: expandedBody, tail, pos) |> Some
                    | _ ->
                        emitAt ctx invalidSyntaxItemUse pos "Ill-formed 'lambda'"
                        ctx, None

                // `define`: rename the introduced name into the *outer* env so
                // subsequent top-level forms can resolve it. The init expression
                // sees the env before the define; the shorthand body includes the
                // function name (enabling recursion).
                | StxBinding.SpecialForm Define ->
                    let keyword = Stx.Symbol(id, idPos)
                    match rest with
                    | [ Stx.Symbol(nameId, namePos); initStx ] ->
                        // Simple (define x val)
                        let newId, outerEnv = StxEnv.rename nameId ctx.Env
                        let expandedInit = expandSimple ctx initStx
                        let outerCtx = { ctx with Env = outerEnv }
                        outerCtx,
                        Stx.Form([ keyword; Stx.Symbol(newId, namePos); expandedInit ], tail, pos)
                        |> Some
                    | Stx.Form(Stx.Symbol(nameId, namePos) :: formalRest, formalTail, formalsPos) :: body ->
                        // Shorthand (define (f args…) body…)
                        // f is renamed into the outer env; formals are renamed into
                        // a fresh inner env that also includes f (for recursion).
                        let newId, outerEnv = StxEnv.rename nameId ctx.Env
                        let outerCtx = { ctx with Env = outerEnv }
                        let renamedArgsForm, innerEnv =
                            renameFormalsPattern outerEnv (Stx.Form(formalRest, formalTail, formalsPos))
                        let innerCtx = { ctx with Env = innerEnv }
                        let fWithArgs =
                            match renamedArgsForm with
                            | Stx.Form(args, argTail, fPos) ->
                                Stx.Form(Stx.Symbol(newId, namePos) :: args, argTail, fPos)
                            | other -> other
                        let expandedBody = expandSimpleLst innerCtx body
                        outerCtx, Stx.Form(keyword :: fWithArgs :: expandedBody, tail, pos) |> Some
                    | _ ->
                        emitAt ctx invalidSyntaxItemUse pos "Ill-formed 'define'"
                        ctx, None

                // `let`: all inits evaluated in the outer env (not seeing each
                // other); bindings scoped only to the body.
                | StxBinding.SpecialForm Let ->
                    let keyword = Stx.Symbol(id, idPos)
                    match rest with
                    | Stx.Form(bindings, None, bindPos) :: body ->
                        let expandBinding (env: StxEnv) (binding: Stx) =
                            match binding with
                            | Stx.Form([ Stx.Symbol(nameId, namePos); initStx ], None, bPos) ->
                                // init sees the outer env (let, not let*)
                                let expandedInit = expandSimple ctx initStx
                                let newId, env' = StxEnv.rename nameId env
                                Stx.Form([ Stx.Symbol(newId, namePos); expandedInit ], None, bPos), env'
                            | _ ->
                                emitAt ctx invalidSyntaxItemUse (Stx.getPos binding) "Ill-formed let binding"
                                binding, env
                        let renamedBindings, innerEnv = List.mapFold expandBinding ctx.Env bindings
                        let innerCtx = { ctx with Env = innerEnv }
                        let expandedBody = expandSimpleLst innerCtx body
                        ctx,
                        Stx.Form(keyword :: Stx.Form(renamedBindings, None, bindPos) :: expandedBody, tail, pos)
                        |> Some
                    | _ ->
                        emitAt ctx invalidSyntaxItemUse pos "Ill-formed 'let'"
                        ctx, None

                // `let*`: each init evaluated in the env extended by all preceding
                // bindings (sequential scoping).
                | StxBinding.SpecialForm LetStar ->
                    let keyword = Stx.Symbol(id, idPos)
                    match rest with
                    | Stx.Form(bindings, None, bindPos) :: body ->
                        let expandBinding (env: StxEnv) (binding: Stx) =
                            match binding with
                            | Stx.Form([ Stx.Symbol(nameId, namePos); initStx ], None, bPos) ->
                                let expandedInit = expandSimple { ctx with Env = env } initStx
                                let newId, env' = StxEnv.rename nameId env
                                Stx.Form([ Stx.Symbol(newId, namePos); expandedInit ], None, bPos), env'
                            | _ ->
                                emitAt ctx invalidSyntaxItemUse (Stx.getPos binding) "Ill-formed let* binding"
                                binding, env
                        let renamedBindings, innerEnv = List.mapFold expandBinding ctx.Env bindings
                        let innerCtx = { ctx with Env = innerEnv }
                        let expandedBody = expandSimpleLst innerCtx body
                        ctx,
                        Stx.Form(keyword :: Stx.Form(renamedBindings, None, bindPos) :: expandedBody, tail, pos)
                        |> Some
                    | _ ->
                        emitAt ctx invalidSyntaxItemUse pos "Ill-formed 'let*'"
                        ctx, None

                // `letrec` / `letrec*`: all binding names are in scope for all
                // inits and the body. (The sequential-initialisation constraint of
                // letrec* is a runtime/binder concern, not the expander's.)
                | StxBinding.SpecialForm(LetRec | LetRecStar) ->
                    let keyword = Stx.Symbol(id, idPos)
                    match rest with
                    | Stx.Form(bindings, None, bindPos) :: body ->
                        let parseSpec (binding: Stx) =
                            match binding with
                            | Stx.Form([ Stx.Symbol(nameId, namePos); initStx ], None, bPos) ->
                                Some(nameId, namePos, initStx, bPos)
                            | _ ->
                                emitAt ctx invalidSyntaxItemUse (Stx.getPos binding) "Ill-formed letrec binding"
                                None
                        let specs = List.choose parseSpec bindings
                        // Rename all binders up-front so each init can see all names
                        let newIds, innerEnv =
                            List.mapFold
                                (fun env (nameId, _, _, _) -> StxEnv.rename nameId env)
                                ctx.Env
                                specs
                        let innerCtx = { ctx with Env = innerEnv }
                        let renamedBindings =
                            List.zip newIds specs
                            |> List.map (fun (newId, (_, namePos, initStx, bPos)) ->
                                let expandedInit = expandSimple innerCtx initStx
                                Stx.Form([ Stx.Symbol(newId, namePos); expandedInit ], None, bPos))
                        let expandedBody = expandSimpleLst innerCtx body
                        ctx,
                        Stx.Form(keyword :: Stx.Form(renamedBindings, None, bindPos) :: expandedBody, tail, pos)
                        |> Some
                    | _ ->
                        emitAt ctx invalidSyntaxItemUse pos "Ill-formed 'letrec'"
                        ctx, None

                // `quote`: the datum is data, not code — do not recurse.
                | StxBinding.SpecialForm Quote -> ctx, Some stx

                // Remaining special forms (if, begin, set!, …): no new bindings
                // are introduced so simply recurse into all subforms.
                | StxBinding.SpecialForm _ -> expandSimpleForm ctx body tail pos

                // Ordinary variable in head position: rename, expand arguments.
                | StxBinding.Var rId ->
                    let newHead = Stx.Symbol(rId, idPos)
                    let expandedRest = expandSimpleLst ctx rest
                    let expandedTail = tail |> Option.map (expandSimple ctx)
                    ctx, Stx.Form(newHead :: expandedRest, expandedTail, pos) |> Some

            // Non-symbol head (e.g. `((lambda (x) x) 1)`): expand all subforms.
            | _ -> expandSimpleForm ctx body tail pos

    /// Expand a `Stx` that must produce a value. If expansion yields `None`
    /// (e.g. a `define-syntax` in expression context) a diagnostic is emitted and
    /// the original node is returned as a fallback.
    and expandSimple (ctx: StxCtx) (stx: Stx) : Stx =

        match expand ctx stx with
        | _, Some result -> result
        | _, None ->
            // This case is hit when we encounter an invalid use of a syntax item,
            // such as using a syntax item as an identifier. In this case we emit
            // a diagnostic and return the original syntax, which will likely
            // cause further errors during binding, but prevents us from emitting
            // a cascade of diagnostics.
            emitAt ctx invalidSyntaxItemUse (Stx.getPos stx)
                "Syntax form expanded to nothing in expression context"

            stx
