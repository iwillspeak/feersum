module Lower

open Bind

type private CaptureConversionCtx = { Parent: CaptureConversionCtx option
                                    ; Mappings: Map<StorageRef,StorageRef> }

let private mappingsForExpr expr =
    Map.empty

/// Re-write a storage location to move captured values to the environment.
let rec private rewriteStorage ctx = function
    | Captured s ->
        match s with
        | Arg _
        | Local _ ->
            ctx.Mappings.[s]
        | Captured c ->
            match ctx.Parent with
            | Some(parent) -> Captured (rewriteStorage parent c)
            | None ->
                failwith "Internal compiler error: Capture chain does not match nesting"
        // in theory this shouldn't happen. Capture chains should only contain
        // args + locals + captures
        | s -> s
    | s -> s

/// Lower a single expression and re-write the capture environment for it.
let rec private rewriteExpression ctx = function
    | Load s -> Load (rewriteStorage ctx s)
    | Store(s, v) -> Store(rewriteStorage ctx s, Option.map (rewriteExpression ctx) v)
    | Application(app, args) -> Application(rewriteExpression ctx app, List.map (rewriteExpression ctx) args)
    | If(cond, cons, els) -> If(rewriteExpression ctx cond, rewriteExpression ctx cons, Option.map (rewriteExpression ctx) els)
    | Seq(exprs) -> Seq(List.map (rewriteExpression ctx) exprs)
    | Lambda(formals, locals, captures, env, body) ->
        // find out what is captured
        let ctx = { Parent = Some(ctx); Mappings = mappingsForExpr body }
        // re-write the body
        Lambda(formals, locals, captures, env, (rewriteExpression ctx body))
    | e -> e
/// Lower a bound tree to a form better suited for the emit phase.
let lower ast =
    let ctx = { Parent = None; Mappings = mappingsForExpr ast.Root }
    { ast with Root = rewriteExpression ctx ast.Root }