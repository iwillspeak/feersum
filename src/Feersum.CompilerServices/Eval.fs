module Feersum.CompilerServices.Eval

open System.IO
open System.Reflection
open System
open System.Runtime.ExceptionServices

open Serehfa

open Feersum.CompilerServices
open Feersum.CompilerServices.Compile

/// The default set of otpions for scripting
let defaultScriptOptions = CompilationOptions.Create Debug Script

/// Raw External Representation
///
/// Returns the external representation for a CIL type.
let cilExternalRepr (object: Object) = Write.GetExternalRepresentation(object)

/// Invoke a successfully-emitted assembly's script body.
let private invokeAssembly (memStream: MemoryStream) =
    let assm = Assembly.Load(memStream.ToArray())
    let progTy = assm.GetType "evalCtx.LispProgram"
    let mainMethod = progTy.GetMethod "$ScriptBody"

    try
        Ok(mainMethod.Invoke(null, Array.empty<obj>))
    with :? TargetInvocationException as ex ->
        // Unwrap target invocation exceptions to give the REPL a nicer experience.
        ExceptionDispatchInfo.Capture(ex.InnerException).Throw()
        Error []

/// Evaluate a script, deriving scope from a previous compilation when given.
///
/// Returns the runtime result paired with the compilation to pass to the next
/// step. On a bind error the `previous` compilation is returned unchanged so
/// the caller's accumulated scope is preserved.
let evalWithPrev (previous: Compilation option) options input =
    let c =
        match previous with
        | None -> Compilation.create options input
        | Some prev -> Compilation.createDerived prev input

    if not c.BoundTree.Diagnostics.IsEmpty then
        Error c.BoundTree.Diagnostics, previous
    else
        use memStream = new MemoryStream()
        Compilation.emit c memStream "evalCtx" None |> ignore
        invokeAssembly memStream |> Result.mapError (fun _ -> []), Some c

/// Take a syntax tree and evaluate it in-process.
///
/// Compiles to an in-memory assembly and invokes the script body. Uses
/// `options` for compilation settings. Does not chain scope between calls;
/// use `evalWithPrev` for REPL state accumulation.
let evalWith options input = evalWithPrev None options input |> fst

/// Evaluate using the default script options.
let eval = evalWith defaultScriptOptions
