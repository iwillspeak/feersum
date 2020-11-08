module Macros
open Syntax
open Diagnostics
open Utils

/// The macro pattern type. Used in syntax cases to define the form that a
/// macro should match.
type MacroPattern =
    | Constant of SyntaxConstant
    | Underscore
    | Literal of string
    | Variable of string
    | Repeat of MacroPattern
    | Form of MacroPattern list
    | DottedForm of MacroPattern list * MacroPattern

/// The binding of a macro variable to syntax.
type MacroBinding = (string * AstNode)

/// Create an error result with a diagnostic at `location`
let private errAt location message =
    Diagnostic(location, message)
    |> Result.Error

/// Attempt to match a pattern against a syntax tree. Returns `Ok` if the
/// pattern matches. Returns `Err` if the pattern does not match the given node.
let rec macroMatch (pat: MacroPattern) (ast: AstNode): Result<MacroBinding list,unit> =
    match pat with
    | Constant c ->
        match ast.Kind with
        | AstNodeKind.Constant k ->
            if k = c then
                Result.Ok []
            else
                Result.Error ()
        | _ -> Result.Error ()
    | Variable v ->
        Result.Ok [(v, ast)]
    | Form patterns ->
        match ast.Kind with
        | AstNodeKind.Form g ->
            matchForm patterns None g
        | _ -> Result.Error ()
    | DottedForm(patterns, tail) ->
        match ast.Kind with
        | AstNodeKind.Form g ->
            matchForm patterns (Some(tail)) g
        | _ -> Result.Error ()
    | Underscore -> Result.Ok []
    | Literal literal ->
        match ast.Kind with
        | AstNodeKind.Ident id ->
            if id = literal then
                Result.Ok []
            else
                Result.Error ()
        | _ -> Result.Error ()
    | Repeat _ ->
        // We _could_ try and 'gracefully' fail here by just matching the inner
        // pattern. That would make finding any bugs in our pattern parsing more
        // difficult to track down though.
        failwith "Repeat at top level"

/// Try to match a list of patterns and, optionally, a tail form against the
/// contents of a form. If the pattern list contains any elipsis the heavy
/// is forwarded to `matchRepeated`.
and private matchForm patterns maybeTail syntax =
    match patterns with
    | MacroPattern.Repeat(repeat)::pats ->
        matchRepeated repeat pats maybeTail syntax
    | headPat::patterns ->
        match syntax with
        | head::rest ->
            macroMatch headPat head
            |> Result.bind (fun vars ->
                matchForm patterns maybeTail rest
                |> Result.map (List.append vars))
        | [] -> Result.Error ()
    | [] ->
        match maybeTail with
        | Some tailPattern ->
            match syntax with
            | [single] -> 
                macroMatch tailPattern single
            | other ->
                match tailPattern with
                | Underscore -> Result.Ok []
                 // FIXME: vv need a way to bind this vv
                | Variable _ -> Result.Ok []
                | _ -> Result.Error ()
        | None ->
            if List.isEmpty syntax then
                Ok []
            else
                Result.Error ()

/// Try and match a repeated pattern. This effectively re-matches the tail
/// patterns until no further matches of repeat are required in a manner similar
/// to backtracking.
and private matchRepeated repeat patterns maybeTail syntax =
    match matchForm patterns maybeTail syntax with
    | Ok vars -> Ok vars
    | _ ->
        match syntax with
        | head::syntax ->
            macroMatch repeat head
            |> Result.bind (fun x ->
                matchRepeated repeat patterns maybeTail syntax
                |> Result.map (List.append x))
        | _ ->
            // Ran out of repeats and tail never matched
            Result.Error ()

/// Try to parse a pattern from the given syntax.
let rec parsePattern literals syntax =
    let recurse = parsePattern literals
    let e = errAt syntax.Location
    match syntax.Kind with
    | AstNodeKind.Constant c -> Ok(MacroPattern.Constant c)
    | AstNodeKind.Dot -> e "Unexpected dot"
    | AstNodeKind.Ident id ->
        match id with
        | "_" -> MacroPattern.Underscore
        | l when List.contains l literals ->
            MacroPattern.Literal l
        | v -> MacroPattern.Variable v
        |> Ok
    | AstNodeKind.Form f ->
        let rec parseForm dotLoc nodes =
            match dotLoc with
            | Some loc ->
                ([],match nodes with
                    | [n] -> recurse n
                    | _ ->
                        errAt loc "Only expected a single pattern after dot"
                |> Some)
            | _ ->
                match nodes with
                | { Kind = AstNodeKind.Dot; Location = l }::rest ->
                    parseForm (Some(l)) rest
                | node::rest ->
                    let pat = recurse node
                    let (pat, rest) = 
                        match rest with
                        | { Kind = AstNodeKind.Ident("...") }::rest ->
                            (pat |> Result.map MacroPattern.Repeat, rest)
                        | _ -> (pat, rest)
                    let (pats, dotPat) = parseForm dotLoc rest
                    (pat::pats, dotPat) 
                | [] -> ([], None)

        let (pats, dotPat) = parseForm None f
        match dotPat with
        | Some dot ->
            pats
            |> ResultEx.collect
            |> Result.bind (fun pats ->
                match dot with 
                | Ok d -> Ok(MacroPattern.DottedForm(pats, d))
                | _ -> dot)
        | None ->
            pats |> ResultEx.collect |> Result.map MacroPattern.Form
    | AstNodeKind.Vector _ | AstNodeKind.ByteVector _ | AstNodeKind.Quoted _ ->
        e "Unsupported pattern element"
    | AstNodeKind.Seq _ | AstNodeKind.Error ->
        e "Invalid macro pattern"