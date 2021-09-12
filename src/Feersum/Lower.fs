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
    | Lambda(formals, body) ->
        let isFree = function
            | Arg _ | Local _ -> true
            | _ -> false
        body.Captures |> List.filter isFree
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
        Map.tryFind s ctx.Mappings |> Option.defaultValue s

/// Re-write the environment size, if needed.
let private rewriteEnv env mappings =
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
    | Lambda(formals, root) ->
        Lambda(formals, (rewriteRoot (Some(ctx)) root))
    | SequencePoint(inner, location) ->
        SequencePoint((rewriteExpression ctx inner), location)
    | Library(name, body) ->
        Library(name, rewriteRoot None body)
    | e -> e
and private rewriteRoot parent root =
    // First re-write our free variables. This is in reference to the parent
    // context so needs to be done before we create a derived `ctx`.
    let captures =
        match parent with
        | Some(ctx) -> List.map (rewriteStorage ctx) root.Captures
        | _ -> root.Captures
    // find out what is captured
    let ctx = { Parent = parent; Mappings = mappingsForExpr root.Body }
    /// Update our environment size if captures were found
    let env = rewriteEnv root.EnvMappings ctx.Mappings
    // re-constitute
    { Body = (rewriteExpression ctx root.Body)
    ; Locals = root.Locals
    ; Captures = captures
    ; EnvMappings = env }

/// Lower a bound tree to a form better suited for the emit phase.
let lower (ast: BoundSyntaxTree) =
    { ast with Root = rewriteRoot None ast.Root }