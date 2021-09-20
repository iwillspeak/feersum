module Eval

open System.IO
open System.Reflection
open System
open System.Runtime.ExceptionServices

open Serehfa

open Compile
open Options

/// Raw External Representation
///
/// Returns the external representation for a CIL type.
let cilExternalRepr (object: Object) =
    Write.GetExternalRepresentation(object)

/// Take a syntax tree and evaluate it in-process
///
/// This first compiles the tree to an in-memory assembly and then calls the
/// main method on that.
let eval ast =
    let memStream = new MemoryStream()
    let options = CompilationOptions.Create Debug  Script
    let diags = compile options memStream "evalCtx" None ast
    if not diags.IsEmpty then
        Error(diags)
    else
        let assm = Assembly.Load(memStream.ToArray())
        let progTy = assm.GetType("evalCtx.LispProgram")
        let mainMethod = progTy.GetMethod("$ScriptBody")
        try
            Ok(mainMethod.Invoke(null, Array.empty<obj>))
        with
        | :? TargetInvocationException as ex  ->
            // Unwrap target invocation exceptions a little to make the REPL a
            // bit of a nicer experience
            ExceptionDispatchInfo.Capture(ex.InnerException).Throw();
            Error([])