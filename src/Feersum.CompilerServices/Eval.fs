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
type ReplLoadContext() =
    inherit AssemblyLoadContext(isCollectible = false)
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

/// A session-scoped evaluation context. Holds compilation options, the
/// shared assembly load context for REPL cross-step references, and the
/// accumulated `CompilationOutput` from prior steps.
///
/// Dispose when the session ends to release the load context.
[<NoComparison; NoEquality>]
type EvalContext =
    { Options: CompilationOptions
      LoadContext: ReplLoadContext
      mutable State: CompilationOutput option }

    interface IDisposable with
        member _.Dispose() = ()

module EvalContext =
    let create options =
        { Options = options
          LoadContext = ReplLoadContext()
          State = None }

/// Invoke a successfully-emitted assembly's script body via the given load context.
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
/// previous step stored in `ctx.State`. Updates `ctx.State` on success.
let evalInContext (ctx: EvalContext) input =
    let nextIndex = ctx.State |> Option.map (fun s -> s.StepIndex + 1) |> Option.defaultValue 0

    let compilation =
        match ctx.State with
        | None -> Compilation.create ctx.Options input
        | Some prev -> Compilation.createDerived prev input

    if not compilation.BoundTree.Diagnostics.IsEmpty then
        Error compilation.BoundTree.Diagnostics
    else
        use memStream = new MemoryStream()
        // Each step gets a unique assembly name so the load context can hold
        // all steps simultaneously without name collisions.
        let assmStem = sprintf "evalCtx_%d" nextIndex
        let output = Compilation.emit compilation memStream (assmStem + ".dll") None
        let typeName = sprintf "%s.%s" assmStem compilation.BoundTree.MangledName
        let bytes = memStream.ToArray()

        match invokeAssembly ctx bytes typeName with
        | Ok value ->
            ctx.State <- Some { output with StepIndex = nextIndex }
            Ok value
        | Error diags -> Error diags

/// Evaluate using the given options without retaining any session state.
let evalWith (options: CompilationOptions) input =
    use ctx = EvalContext.create options
    evalInContext ctx input

/// Evaluate using the default script options.
let eval = evalWith defaultScriptOptions
