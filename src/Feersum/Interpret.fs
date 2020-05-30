module Interpret

open Syntax
open Bind

/// Shceme value
///
/// Represents the different types that a given value can have
/// in the interpreter.
type SchemeValue =
    | Nil
    | Undefined
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
 
/// Get a map with the builtin values
/// 
/// This only handles a small set of builtin identifiers. It is used
/// to create the global scope for execution.
let builtins =
    [ "+",  numberBinop(+)
    ; "-",  numberBinop(-)
    ; "*",  numberBinop(*)
    ; "/",  numberBinop(/)         
    ] |> Map.ofList

let rec executeBound (expr: BoundExpr) =
    match expr with
    | BoundExpr.Null -> SchemeValue.Nil
    | BoundExpr.Number n -> SchemeValue.Number n
    | BoundExpr.Str s -> SchemeValue.Str s
    | BoundExpr.Boolean b ->  SchemeValue.Boolean b
    | BoundExpr.Seq s ->
        s
        |> List.map executeBound
        |> List.tryLast
        |> Option.defaultValue SchemeValue.Undefined
    | BoundExpr.Application(ap, args) ->
        apply (executeBound ap) (List.map executeBound args)
    | BoundExpr.Load l ->
        match l with
        | StorageRef.Global id ->  Map.tryFind id builtins |> Option.get
        | StorageRef.Local idx -> 
            // TODO: implement local and global loads
            failwith "local loads not implemented in the interpreter"
    | BoundExpr.If(cond, ifTrue, maybeIfFalse) ->
        match (executeBound cond) with
        | SchemeValue.Boolean false ->
            maybeIfFalse
            |> Option.map executeBound
            |> Option.defaultValue SchemeValue.Undefined
        | _ -> executeBound ifTrue

/// Take a syntax tree and evaluate it producing a value.
let execute (input: AstNode): SchemeValue =
    let scope: Map<string, StorageRef> =
        Map.fold (fun s id _ -> s.Add(id, StorageRef.Global id)) createRootScope builtins
    bind scope input |> executeBound

/// Take a Scheme Value and convert it to the
/// 'external representation' as a string
let externalRepr value =
    match value with
    | Undefined -> "; unspecified value"
    | Nil -> "NIL"
    | Number n -> n.ToString("g")
    | Str s -> sprintf "%A" s
    | Boolean b -> if b then "#t" else "#f"
    | Builtin f -> "#[procedure]"
    | Quoted q -> sprintf "%A" q
