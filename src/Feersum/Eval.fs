module Eval

open Syntax
open Bind
open Compile
open System.IO
open System.Reflection
open System

/// Take a syntax tree and evaluate it in-process
///
/// This first compiles the tree to an in-memory assembly and then calls the
/// main method on that.
let eval ast =
    let memStream = new MemoryStream()
    compile memStream "evalCtx" ast
    let assm = Assembly.Load(memStream.ToArray())
    let progTy = assm.GetType("evalCtx.LispProgram")
    let mainMethod = progTy.GetMethod("Main")
    mainMethod.Invoke(null, [| Array.empty<String> |])