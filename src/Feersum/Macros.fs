module Macros
open Syntax

// TODO: Macro patterns shold support `...` matches
// TODO: Macro patterns should support dotted lists `( . )`
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
    let syntax = List.rev syntax
    let patterns = List.rev patterns

    match syntax with
    | tail::rest ->
        match macroMatch tailPattern tail with
        | Ok tailVars ->
            List.map2 (macroMatch) patterns rest
            |> collectResults
            |> Result.map (List.collect id)
            |> Result.map (List.append tailVars)
        | e -> e
    | [] -> Result.Error ()