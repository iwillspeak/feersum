module Feersum.CompilerServices.Binding.Lower

open Feersum.CompilerServices.Ice

type private CaptureConversionCtx =
    { Parent: CaptureConversionCtx option
      Mappings: Map<StorageRef, StorageRef> }

/// Find the storage references that have been captured in the body of a given
/// expression. The returned list only contains _directly_ captured values.
let rec private findCaptured =
    function
    | Application (app, args) -> List.append (findCaptured app) (List.collect findCaptured args)
    | If (cond, cons, els) ->
        (findCaptured cond)
        |> List.append (findCaptured cons)
        |> List.append (
            Option.map findCaptured els
            |> Option.defaultValue []
        )
    | Seq (exprs) -> List.collect findCaptured exprs
    | Lambda (formals, body) ->
        let isFree =
            function
            | Arg _
            | Local _ -> true
            | _ -> false

        body.Captures |> List.filter isFree
    | Store (_, v) ->
        Option.map findCaptured v
        |> Option.defaultValue []
    | SequencePoint (inner, _) -> findCaptured inner
    | e -> []

/// Finds all local variables in the given node. This is later used to compact
/// down local declarations to remove 'shadow locals' from captures.
let rec private findLocals node =
    let fromStorage =
        function
        | Local x -> Set.singleton (Local x)
        | _ -> Set.empty

    match node with
    | Load (storage) -> fromStorage storage
    | Application (app, args) -> Set.union (findLocals app) (Seq.map (findLocals) args |> Set.unionMany)
    | If (cond, cons, els) ->
        (findLocals cond)
        |> Set.union (findLocals cons)
        |> Set.union (
            Option.map (findLocals) els
            |> Option.defaultValue Set.empty
        )
    | Seq (exprs) -> Seq.map (findLocals) exprs |> Set.unionMany
    | SequencePoint (inner, _) -> findLocals inner
    | Store (storage, v) ->
        let inner =
            Option.map (findLocals) v
            |> Option.defaultValue Set.empty

        Set.union (fromStorage storage) inner
    | e -> Set.empty

/// Finds the variable references that need to be moved into the environment at
/// this level.
let private mappingsForExpr expr =
    let captured = findCaptured expr |> Set.ofSeq

    let captureReWrites =
        captured
        |> Seq.indexed
        |> Seq.map (fun (i, s) -> (s, StorageRef.Environment(i, s)))

    let uncapturedLocals =
        Set.difference (findLocals expr) captured

    let localReWrites =
        uncapturedLocals
        |> Seq.indexed
        |> Seq.map (fun (i, s) -> (s, Local i))

    Seq.append captureReWrites localReWrites
    |> Map.ofSeq

/// Re-write a storage location to move captured values to the environment.
let rec private rewriteStorage ctx =
    function
    | Captured s ->
        match ctx.Parent with
        | Some (parent) -> Captured(rewriteStorage parent s)
        | None -> ice "Capture chain does not match nesting"
    | s ->
        Map.tryFind s ctx.Mappings
        |> Option.defaultValue s

/// Re-write the environment size, if needed.
let private rewriteEnv env mappings =
    if Map.isEmpty mappings then
        env
    else
        Map.toSeq mappings
        |> Seq.where (fun (_, s) ->
            match s with
            | Environment _ -> true
            | _ -> false)
        |> Seq.map (fun (_, v) -> v)
        |> List.ofSeq
        |> Some

/// Re-write the number of locals based on the `mappings`.
let private rewriteLocals locals mappings =
    if Map.isEmpty mappings then
        locals
    else
        Map.toSeq mappings
        |> Seq.sumBy (fun (_, s) ->
            match s with
            | Local _ -> 1
            | _ -> 0)

/// Lower a single expression and re-write the capture environment for it.
let rec private rewriteExpression ctx =
    function
    | Load s -> Load(rewriteStorage ctx s)
    | Store (s, v) -> Store(rewriteStorage ctx s, Option.map (rewriteExpression ctx) v)
    | Application (app, args) -> Application(rewriteExpression ctx app, List.map (rewriteExpression ctx) args)
    | If (cond, cons, els) ->
        If(rewriteExpression ctx cond, rewriteExpression ctx cons, Option.map (rewriteExpression ctx) els)
    | Seq (exprs) ->
        // This pair of functions allows us to flatten out nested sequences into
        // a simpler representation.
        let consolidate =
            function
            | [] -> Nop
            | [ single ] -> single
            | other -> Seq(other)

        let unfurl =
            function
            | Seq inner -> inner
            | Nop -> []
            | o -> [ o ]

        exprs
        |> Seq.map ((rewriteExpression ctx) >> unfurl)
        |> List.concat
        |> consolidate
    | Lambda (formals, root) -> Lambda(formals, (rewriteRoot (Some(ctx)) root))
    | SequencePoint (inner, location) -> SequencePoint((rewriteExpression ctx inner), location)
    | Library (name, mangledName, exports, body) -> Library(name, mangledName, exports, rewriteRoot None body)
    | e -> e

/// Re-write an expression tree root. This node represents a top level
/// context that will eventually be emitted as a method body. Exmaples
/// include script bodies, library bodies, and lambda bodies.
and private rewriteRoot parent root =
    // First re-write our free variables. This is in reference to the parent
    // context so needs to be done before we create a derived `ctx`.
    let captures =
        match parent with
        | Some (ctx) -> List.map (rewriteStorage ctx) root.Captures
        | _ -> root.Captures
    // find out what is captured
    let ctx =
        { Parent = parent
          Mappings = mappingsForExpr root.Body }
    /// Update our environment and local size if captures were found
    let env = rewriteEnv root.EnvMappings ctx.Mappings
    let locals = rewriteLocals root.Locals ctx.Mappings
    // re-constitute
    { Body = (rewriteExpression ctx root.Body)
      Locals = locals
      Captures = captures
      EnvMappings = env }

/// Lower a bound tree to a form better suited for the emit phase.
let lower (ast: BoundSyntaxTree) =
    { ast with Root = rewriteRoot None ast.Root }
