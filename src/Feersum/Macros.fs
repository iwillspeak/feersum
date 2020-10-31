module Macros
open Syntax

/// The macro pattern type. Used in syntax cases to define the form that a
/// macro should match.
type MacroPattern =
    | Constant of SyntaxConstant
    | Variable of string

type MacroBinding = (string * AstNode)

/// Attempt to match a pattern against a syntax tree. Returns `Ok` if the
/// pattern matches. Returns `Err` if the pattern does not match the given node.
let macroMatch (pat: MacroPattern) (ast: AstNode): Result<MacroBinding list,unit> =
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