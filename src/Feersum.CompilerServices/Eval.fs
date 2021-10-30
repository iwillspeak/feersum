module Feersum.CompilerServices.Eval

open System.IO
open System.Reflection
open System
open System.Runtime.ExceptionServices

open Serehfa

open Feersum.CompilerServices.Compile
open Feersum.CompilerServices.Options

/// The default set of otpions for scripting
let defaultScriptOptions = CompilationOptions.Create Debug Script

/// Raw External Representation
///
/// Returns the external representation for a CIL type.
let cilExternalRepr (object: Object) = Write.GetExternalRepresentation(object)

/// Take a syntax tree and evaluate it in-process
///
/// This first compiles the tree to an in-memory assembly and then calls the
/// main method on that.
let evalWith options ast =
    let memStream = new MemoryStream()

    let result =
        Compilation.compile options memStream "evalCtx" None ast

    if not result.Diagnostics.IsEmpty then
        Error(result.Diagnostics)
    else
        let assm = Assembly.Load(memStream.ToArray())
        let progTy = assm.GetType("evalCtx.LispProgram")
        let mainMethod = progTy.GetMethod("$ScriptBody")

        try
            Ok(mainMethod.Invoke(null, Array.empty<obj>))
        with
        | :? TargetInvocationException as ex ->
            // Unwrap target invocation exceptions a little to make the REPL a
            // bit of a nicer experience
            ExceptionDispatchInfo
                .Capture(ex.InnerException)
                .Throw()

            Error([])

let eval = evalWith defaultScriptOptions