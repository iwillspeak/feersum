module Feersum.CompilerServices.Eval

open System.Collections.Generic
open System.IO
open System.Reflection
open System
open System.Runtime.ExceptionServices
open System.Runtime.Loader

open Serehfa

open Feersum.CompilerServices
open Feersum.CompilerServices.Compile

/// The default set of options for scripting
let defaultScriptOptions = CompilationOptions.Create Debug Script

/// Raw External Representation
///
/// Returns the external representation for a CIL type.
let cilExternalRepr (object: Object) = Write.GetExternalRepresentation(object)

/// An AssemblyLoadContext that retains every assembly loaded into it by name,
/// so that cross-step references in a REPL session resolve correctly.
/// Collectible so the entire session can be unloaded when disposed.
type ReplLoadContext() =
    inherit AssemblyLoadContext(isCollectible = true)
    let loaded = Dictionary<string, Assembly>()

    member this.LoadBytes(bytes: byte[]) =
        use ms = new MemoryStream(bytes)
        let assm = this.LoadFromStream ms
        loaded.[assm.GetName().Name] <- assm
        assm

    override _.Load name =
        match loaded.TryGetValue name.Name with
        | true, a -> a
        | _ -> null

/// A session-scoped evaluation context. Immutable value: `evalInContext`
/// returns a new context rather than mutating the existing one.
///
/// The `LoadContext` is a shared reference type across all derived contexts.
/// Call `Dispose` on the original context when the session ends to unload it.
[<NoComparison; NoEquality>]
type EvalContext =
    { Options: CompilationOptions
      LoadContext: ReplLoadContext
      State: CompilationOutput option
      StepIndex: int }

    interface IDisposable with
        member ctx.Dispose() = ctx.LoadContext.Unload()

module EvalContext =
    let create options =
        { Options = options
          LoadContext = ReplLoadContext()
          State = None
          StepIndex = 0 }

/// Invoke the script body of an emitted assembly via the session load context.
let private invokeAssembly (ctx: EvalContext) (bytes: byte[]) (typeName: string) =
    let assm = ctx.LoadContext.LoadBytes bytes
    let progTy = assm.GetType typeName
    let mainMethod = progTy.GetMethod "$ScriptBody"

    try
        Ok(mainMethod.Invoke(null, Array.empty<obj>))
    with :? TargetInvocationException as ex ->
        ExceptionDispatchInfo.Capture(ex.InnerException).Throw()
        Error []

/// Evaluate a script inside a given `EvalContext`, chaining scope from any
/// previous step stored in `ctx.State`.
///
/// Returns the result and a new `EvalContext` with updated state. On bind
/// errors the original context is returned unchanged so the caller's
/// accumulated scope is preserved.
let evalInContext (ctx: EvalContext) input =
    let compilation =
        match ctx.State with
        | None -> Compilation.create ctx.Options input
        | Some prev ->
            let programName = sprintf "LispProgram_%d" ctx.StepIndex
            Compilation.createChained prev programName input

    if not compilation.BoundTree.Diagnostics.IsEmpty then
        Error compilation.BoundTree.Diagnostics, ctx
    else
        use memStream = new MemoryStream()
        // Each step gets a unique assembly name so the load context can hold
        // all steps simultaneously without name collisions.
        let assmStem = sprintf "evalCtx_%d" ctx.StepIndex
        let output = Compilation.emit compilation memStream (assmStem + ".dll") None
        let typeName = sprintf "%s.%s" assmStem compilation.BoundTree.MangledName
        let bytes = memStream.ToArray()

        match invokeAssembly ctx bytes typeName with
        | Ok value ->
            let nextCtx =
                { ctx with
                    State = Some output
                    StepIndex = ctx.StepIndex + 1 }

            Ok value, nextCtx
        | Error diags -> Error diags, ctx

/// Evaluate using the given options without retaining any session state.
let evalWith (options: CompilationOptions) input =
    use ctx = EvalContext.create options
    evalInContext ctx input |> fst

/// Evaluate using the default script options.
let eval = evalWith defaultScriptOptions
