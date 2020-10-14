module Interpret

open System.Collections.Generic

open Bind
open Diagnostics
open Syntax

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

module SchemeValue =

    /// Convenience method to convert an optional value
    ///
    /// If the value is `None` then map to `Undefined`, otherwise
    /// unwrap the inner value.
    let fromOption maybeValue =
        Option.defaultValue SchemeValue.Undefined maybeValue

/// Number Binary Operation
///
/// Create a builtin scheme value which reduces numeric parameters
/// using `op`
let numberBinop op =
    let builtin = List.reduce (fun a b ->
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

let rec executeBound (env: IDictionary<string, SchemeValue>) (expr: BoundExpr) =
    let recurse = executeBound env
    match expr with
    | BoundExpr.Null -> SchemeValue.Nil
    | BoundExpr.Number n -> SchemeValue.Number n
    | BoundExpr.Str s -> SchemeValue.Str s
    | BoundExpr.Boolean b ->  SchemeValue.Boolean b
    | BoundExpr.Store(store, maybeInit) ->
        let init = Option.map recurse maybeInit |> SchemeValue.fromOption
        match store with
        | StorageRef.Global id -> env.Add(id, init)
        | _ -> failwithf "Storage not supported %A" store
        // actually using this value is difficult in Scheme so i'm  not sure it
        // matters _too_ much _what_ it is. It's definitely _not_ the init value
        // fo the new variable binding though.
        SchemeValue.Undefined
    | BoundExpr.Seq s ->
        s
        |> List.map recurse
        |> List.tryLast
        |> SchemeValue.fromOption
    | BoundExpr.Application(ap, args) ->
        apply (recurse ap) (List.map recurse args)
    | BoundExpr.Load l ->
        match l with
        | StorageRef.Global id ->  Map.tryFind id builtins |> Option.get
        | _ -> failwithf "Storage not supported %A" l
    | BoundExpr.If(cond, ifTrue, maybeIfFalse) ->
        match (recurse cond) with
        | SchemeValue.Boolean false ->
            maybeIfFalse
            |> Option.map recurse
            |> SchemeValue.fromOption
        | _ -> recurse ifTrue
    | b -> failwithf "%A is not supported" b

/// Take a syntax tree and evaluate it producing a value.
let execute (input: AstNode): Result<SchemeValue, Diagnostic list> =
    let globals = builtins
    let scope: Map<string, StorageRef> =
        Map.fold (fun s id _ -> s.Add(id, StorageRef.Global id)) createRootScope globals
    let initlalEnv = Map.toSeq globals |> dict
    let bound = bind scope input
    if not (Diagnostics.hasErrors bound.Diagnostics) then
        Ok(executeBound initlalEnv bound.Root)
    else
        Result.Error(bound.Diagnostics)

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
