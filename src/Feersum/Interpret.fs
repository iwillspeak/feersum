module Interpret

open Syntax

/// Shceme value
///
/// Represents the different types that a given value can have
/// in the interpreter.
type SchemeValue =
    | Nil
    | Number of double
    | Str of string
    | Boolean of bool
    | Builtin of (SchemeValue list -> SchemeValue)
    | Quoted of AstNode

/// Number Binary Operation
///
/// Create a builtin scheme value which reduces numeric parameters
/// using `op`
let numberBinop op =
    let builtin = fun values ->
        values |> List.reduce (fun a b ->
            match (a, b) with
            | (Number n, Number m) -> Number (op n m)
            | _ -> Nil)
    Builtin builtin

/// Apply a Value to Arguments
/// 
/// Applies the callable `value` to the given `args` and returns the
/// scheme value that results.
let apply value args =
    match value with
    | Builtin f -> f(args)
    | _ -> Nil
 
/// Lookup an identifier
/// 
/// This only handles a small set of builtin identifiers
let lookup ident =
    match ident with
    | "+" ->  numberBinop(+)
    | "-" ->  numberBinop(-)
    | "*" ->  numberBinop(*)
    | "/" ->  numberBinop(/)
    | _ -> Nil

/// Take a syntax tree and evaluate it producing a value.
let rec execute (input: AstNode): SchemeValue =
    let execSeq seq =
        seq |> List.map execute |> List.tryLast |> Option.defaultValue SchemeValue.Nil
    match input with
    | AstNode.Number n  -> SchemeValue.Number n
    | AstNode.Str s -> SchemeValue.Str s
    | AstNode.Boolean b -> SchemeValue.Boolean b
    | AstNode.Seq exprs -> exprs |> execSeq
    | AstNode.Form(AstNode.Ident "begin"::b) -> b |> execSeq
    | AstNode.Form([AstNode.Ident "quote"; e]) -> SchemeValue.Quoted e
    | AstNode.Form([AstNode.Ident "if"; cond; ifTrue; ifFalse]) ->
        match (execute cond) with
        | SchemeValue.Boolean false -> execute ifFalse
        | _ -> execute ifTrue
    | AstNode.Form(head::rest) -> apply (execute head) (List.map execute rest)
    | AstNode.Form([]) -> SchemeValue.Nil
    | AstNode.Ident i -> lookup i

/// Take a Scheme Value and convert it to the
/// 'external representation' as a string
let externalRepr value =
    match value with
    | Nil -> "NIL"
    | Number n -> n.ToString("g")
    | Str s -> sprintf "%A" s
    | Boolean b -> if b then "#t" else "#f"
    | Builtin f -> "#[procedure]"
    | Quoted q -> sprintf "%A" q
