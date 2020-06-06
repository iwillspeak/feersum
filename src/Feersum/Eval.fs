module Eval

open Syntax
open Bind
open Compile
open System.IO
open System.Reflection
open System

/// Raw External Representation
///
/// Returns the external representation for a CIL type.
let cilExternalRepr (object: Object) =
    match object with
    | :? Boolean as b -> if b then "#t" else "#f"
    | :? Double as d -> sprintf "%g" d
    | null -> "()"
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
    // TODO: Instead of calling `$ScriptBody` here should we bind a custom
    //       function definition and call that instead? e.g.: 
    //       
    //       ```scheme
    ///      (define (evalEntry) <ast>)
    ///      ```
    let mainMethod = progTy.GetMethod("$ScriptBody")
    mainMethod.Invoke(null, Array.empty)