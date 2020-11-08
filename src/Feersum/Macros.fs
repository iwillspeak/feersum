module Macros
open Syntax
open Diagnostics

// TODO: Macro patterns shold support `...` matches
/// The macro pattern type. Used in syntax cases to define the form that a
/// macro should match.
type MacroPattern =
    | Constant of SyntaxConstant
    | Underscore
    | Literal of string
    | Variable of string
    | Form of MacroPattern list
    | DottedForm of MacroPattern list * MacroPattern
    
type MacroBinding = (string * AstNode)

let private collectResults (input: Result<'a,'b> list): Result<'a list, 'b> =
    let rec decompose = function
        | [] -> ([],None)
        | Result.Error e::_ -> ([],Some(e))
        | Result.Ok v::rest ->
            let (results,err) = decompose rest
            (v::results,err)
    let (results, maybeErr) = decompose input
    match maybeErr with
    | Some e -> Result.Error e
    | None -> Result.Ok results

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
    | Form f ->
        match ast.Kind with
        | AstNodeKind.Form g ->
            List.map2 (macroMatch) f g
            |> collectResults
            |> Result.map (List.collect id)
            
        | _ -> Result.Error ()
    | DottedForm(patterns, tail) ->
        match ast.Kind with
        | AstNodeKind.Form g ->
            matchDottedForm patterns tail g
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

and matchDottedForm patterns tailPattern syntax =
    match patterns with
    | headPat::patterns ->
        match syntax with
        | head::rest ->
            match macroMatch headPat head with
            | Ok vars ->
                matchDottedForm patterns tailPattern rest
                |> Result.map (List.append vars)
            | e -> e
        | [] -> Result.Error ()
    | [] ->
        match syntax with
        | [single] -> 
            macroMatch tailPattern single
        | other ->
            match tailPattern with
            | Underscore -> Result.Ok []
            | Variable _ -> Result.Ok [] // FIXME: need a way to bind this
            | _ -> Result.Error ()

let rec parsePattern literals syntax =
    let recurse = parsePattern literals
    match syntax.Kind with
    | AstNodeKind.Constant c -> Ok(MacroPattern.Constant c)
    | AstNodeKind.Dot ->
        Diagnostic(syntax.Location, "Unexpected dot")
        |> Result.Error
    | AstNodeKind.Ident id ->
        match id with
        | "_" -> MacroPattern.Underscore
        // TODO: support elipsis
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
                        Diagnostic(loc, "Only expected a single pattern after dot")
                        |> Result.Error
                |> Some)
            | _ ->
                match nodes with
                | { Kind = AstNodeKind.Dot; Location = l }::rest ->
                    parseForm (Some(l)) rest
                | node::rest ->
                    let (pats, dotPat) = parseForm dotLoc rest
                    ((recurse node)::pats, dotPat) 
                | [] -> ([], None)

        let (pats, dotPat) = parseForm None f
        match dotPat with
        | Some dot ->
            pats |> collectResults
            |> Result.bind (fun pats ->
                match dot with 
                | Ok d -> Ok(MacroPattern.DottedForm(pats, d))
                | _ -> dot)
        | None ->
            pats |> collectResults |> Result.map MacroPattern.Form
    | AstNodeKind.Vector _ | AstNodeKind.ByteVector _ | AstNodeKind.Quoted _ ->
        Diagnostic(syntax.Location, "Unsupported pattern element")
        |> Result.Error
    | AstNodeKind.Seq _ | AstNodeKind.Error ->
        Diagnostic(syntax.Location, "Invalid macro pattern")
        |> Result.Error