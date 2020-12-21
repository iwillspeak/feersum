module Lower

open Bind

type private CaptureConversionCtx = { Parent: CaptureConversionCtx option
                                    ; Mappings: Map<StorageRef,StorageRef> }

/// Find the storage references that have been captured in the body of a given
/// expression. The returned list only contains _directly_ captured values.
let rec private findCaptured = function
    | Application(app, args) ->
        List.append (findCaptured app) (List.collect findCaptured args)
    | If(cond, cons, els) ->
        (findCaptured cond)
        |> List.append (findCaptured cons)
        |> List.append (Option.map findCaptured els |> Option.defaultValue [])
    | Seq(exprs) ->
        List.collect findCaptured exprs
    | Lambda(formals, locals, captures, env, body) ->
        let isFree = function
            | Arg _ | Local _ -> true
            | _ -> false
        captures
        |> List.filter isFree
    | Store(_, v) -> Option.map findCaptured v |> Option.defaultValue []
    | SequencePoint(inner, _) -> findCaptured inner
    | e -> []

/// Finds the variable references that need to be moved into the environment at
/// this level.
let private mappingsForExpr expr =
    findCaptured expr
    |> Seq.distinct
    |> Seq.indexed
    |> Seq.map (fun (i, s) -> (s, StorageRef.Environment(i, s)))
    |> Map.ofSeq

/// Re-write a storage location to move captured values to the environment.
let rec private rewriteStorage ctx = function
    | Captured s ->
        match ctx.Parent with
        | Some(parent) ->
            Captured (rewriteStorage parent s)
        | None ->
            failwith "Internal compiler error: Capture chain does not match nesting"
    | s ->
        match Map.tryFind s ctx.Mappings with
        | Some mapped -> mapped
        | None -> s

/// RE-write the environment size, if needed.
let private rewriteEnvSize env mappings =
    if Map.isEmpty mappings then
        env
    else
        Map.toSeq mappings
        |> Seq.map (fun (_, v) -> v)
        |> List.ofSeq
        |> Some

/// Lower a single expression and re-write the capture environment for it.
let rec private rewriteExpression ctx = function
    | Load s -> Load (rewriteStorage ctx s)
    | Store(s, v) -> Store(rewriteStorage ctx s, Option.map (rewriteExpression ctx) v)
    | Application(app, args) -> Application(rewriteExpression ctx app, List.map (rewriteExpression ctx) args)
    | If(cond, cons, els) -> If(rewriteExpression ctx cond, rewriteExpression ctx cons, Option.map (rewriteExpression ctx) els)
    | Seq(exprs) -> Seq(List.map (rewriteExpression ctx) exprs)
    | Lambda(formals, locals, captures, env, body) ->
        // First re-write our free variables. This is in reference to the parent
        // context so needs to be done before we create a derived `ctx`.
        let captures = List.map (rewriteStorage ctx) captures
        // find out what is captured
        let ctx = { Parent = Some(ctx); Mappings = mappingsForExpr body }
        /// Update our environment size if captures were found
        let env = rewriteEnvSize env ctx.Mappings
        // re-write the body
        Lambda(formals, locals, captures, env, (rewriteExpression ctx body))
    | SequencePoint(inner, location) ->
        SequencePoint((rewriteExpression ctx inner), location)
    | e -> e

/// Lower a bound tree to a form better suited for the emit phase.
let lower (ast: BoundSyntaxTree) =
    let ctx = { Parent = None
              ; Mappings = mappingsForExpr ast.Root }
    { ast with Root = rewriteExpression ctx ast.Root
             ; EnvMappings = rewriteEnvSize ast.EnvMappings ctx.Mappings }