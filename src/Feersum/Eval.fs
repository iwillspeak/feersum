module Eval

open Syntax
open Bind
open Compile
open System.IO
open System.Reflection
open System
open System.Runtime.ExceptionServices

/// Raw External Representation
///
/// Returns the external representation for a CIL type.
let cilExternalRepr (object: Object) =
    match object with
    | :? Boolean as b -> if b then "#t" else "#f"
    | :? Double as d -> sprintf "%g" d
    | null -> "()"
    | :? Func<obj[], obj> as f -> sprintf "#<compiledProcedure %A>" f.Method
    | other -> sprintf "%A" other

/// Take a syntax tree and evaluate it in-process
///
/// This first compiles the tree to an in-memory assembly and then calls the
/// main method on that.
let eval ast =
    let memStream = new MemoryStream()
    compile memStream "evalCtx" ast
    let assm = Assembly.Load(memStream.ToArray())
    let progTy = assm.GetType("evalCtx.LispProgram")
    let mainMethod = progTy.GetMethod("$ScriptBody")
    try
        mainMethod.Invoke(null, Array.empty<obj>)
    with
    | :? TargetInvocationException as ex  ->
        // Unwrap target invocation exceptions a little to make the REPL a
        // bit of a nicer experience
        ExceptionDispatchInfo.Capture(ex.InnerException).Throw(); null