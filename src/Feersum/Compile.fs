module Compile

open Bind
open Syntax
open IlHelpers
open System
open System.IO
open Mono.Cecil
open Mono.Cecil.Rocks
open Mono.Cecil.Cil

/// Core Types Used by the Compiler at Runtime
type CoreTypes =
    { ConsTy: TypeDefinition
    ; EnvTy: TypeDefinition }

/// Type to Hold Context While Emitting IL
type EmitCtx =
    { Assm: AssemblyDefinition
    ; IL: ILProcessor
    ; Locals: VariableDefinition list
    ; Parameters: ParameterDefinition list
    ; mutable NextLambda: int
    ; ScopePrefix: string
    ; EnvSize: int option
    ; mutable Environment: VariableDefinition option
    ; ParentEnvSize: int option
    ; ProgramTy: TypeDefinition
    ; Builtins: Map<string,MethodDefinition>
    ; Core: CoreTypes }

/// Create a `ParameterDefinition` with the given `name` and `ty`.
let private namedParam name ty =
    ParameterDefinition(name,
                        ParameterAttributes.None,
                        ty)

let private envSizeForMappings envMappings =
    envMappings
    |> Option.map (fun x ->
        x
        |> Seq.filter (function
            | Environment _ -> true
            | _ -> false)
        |> Seq.length)

/// Set the attributes on a given method to mark it as compiler generated code
let private markAsCompilerGenerated (method: MethodDefinition) =
    let addSimpleAttr (attrTy: Type) =
        method
            .Module
            .ImportReference(attrTy.GetConstructor(Type.EmptyTypes))
        |> CustomAttribute
        |> method.CustomAttributes.Add
    addSimpleAttr typeof<Runtime.CompilerServices.CompilerGeneratedAttribute>
    addSimpleAttr typeof<Diagnostics.DebuggerNonUserCodeAttribute>

/// Emit an instance of the unspecified value
let private emitUnspecified (il: ILProcessor) =
    // TODO: What should we do about empty sequences? This falls back to '()
    il.Emit(OpCodes.Ldnull)

/// Ensure a field exists on the program type to be used as a global variable
let private ensureField ctx id =
    let pred (field: FieldDefinition) =
        field.Name = id
    match Seq.tryFind pred ctx.ProgramTy.Fields with
    | Some(found) -> found
    | None ->
        let newField = FieldDefinition(id, FieldAttributes.Static, ctx.Assm.MainModule.TypeSystem.Object)
        ctx.ProgramTy.Fields.Add(newField)
        newField

/// Convert an argument index into a `ParameterDefinition` for `Ldarg` given the
/// current context. This indexes into the context's `Parameters` list.
let private argToParam ctx idx =
    ctx.Parameters.[idx]

/// Convert a local index into a `VariableDefinition`. Variable definitions can
/// then be used with `Ldloc` and `Stloc`.
let private localToVariable ctx idx =
    ctx.Locals.[idx]

/// Emit a sequence of instructions to convert a method reference
/// into a `Func<obj[],obj>` with the context as the current top of stack.
let private emitMethodToInstanceFunc ctx (method: MethodReference) =
    let paramTypes = [|typeof<obj>; typeof<IntPtr>|]
    let funcObjCtor = typeof<Func<obj[], obj>>.GetConstructor(paramTypes)
    let funcObjCtor = ctx.Assm.MainModule.ImportReference(funcObjCtor)
    ctx.IL.Emit(OpCodes.Ldftn, method)
    ctx.IL.Emit(OpCodes.Newobj, funcObjCtor)

/// Emit a sequence of instructions to convert a method reference
/// into a `Func<obj[],obj>`
let private emitMethodToFunc ctx (method: MethodReference) =
    ctx.IL.Emit(OpCodes.Ldnull)
    emitMethodToInstanceFunc ctx method

/// Make a temporary variable in the current context
let private makeTemp ctx ty =
    let temp = VariableDefinition(ty)
    ctx.IL.Body.Variables.Add <| temp
    temp

/// Create a dynamic environmnet instance for the given context.
let private createEnvironment ctx (size: int) =
    if ctx.ParentEnvSize.IsSome then
        ctx.IL.Emit(OpCodes.Ldarg_0)
    else
        ctx.IL.Emit(OpCodes.Ldnull)
    ctx.IL.Emit(OpCodes.Ldc_I4, size)
    let ctor = Seq.head <| ctx.Core.EnvTy.GetConstructors()
    ctx.IL.Emit(OpCodes.Newobj, ctor)
    let env = makeTemp ctx ctx.Core.EnvTy
    ctx.Environment <- Some env
    ctx.IL.Emit(OpCodes.Stloc, env)

/// Get the variable definition that the local environment is stored in. If the
/// environment hasn't been referenced yet then IL is emitted to initialise it.
let private getEnvironment ctx =
    match ctx.Environment with
    | Some env -> env
    | None ->
        failwith "Internal Compiler Error: Attempt to access environment in context without one"

/// Walk the chain of captures starting at `from` in the parent, and then call
/// `f` with the environment index that the capture chain ends at. This is used
/// to read and write values from a captured environment.
let rec private walkCaptureChain ctx from f =
    match from with
    | StorageRef.Captured(from) ->
        ctx.IL.Emit(OpCodes.Ldfld, ctx.Core.EnvTy.Fields.[0])
        walkCaptureChain ctx from f
    | StorageRef.Environment(idx, _) -> f idx
    | _ -> failwithf "Unexpected storage in capture chain %A" from

/// Given an environment at the top of the stack emit a load of the slot `idx`
let private readFromEnv ctx (idx: int) =
    ctx.IL.Emit(OpCodes.Ldfld, ctx.Core.EnvTy.Fields.[1])
    ctx.IL.Emit(OpCodes.Ldc_I4, idx)
    ctx.IL.Emit(OpCodes.Ldelem_Ref)

/// Given an environment at the top of the stack emit a store to the slot `idx`
let private writeToEnv ctx (temp: VariableDefinition) (idx: int) =
    ctx.IL.Emit(OpCodes.Ldfld, ctx.Core.EnvTy.Fields.[1])
    ctx.IL.Emit(OpCodes.Ldc_I4, idx)
    ctx.IL.Emit(OpCodes.Ldloc, temp)
    ctx.IL.Emit(OpCodes.Stelem_Ref)

/// Emit a Single Bound Expression
///
/// Emits the code for a single function into the given assembly. For some more
/// complex expression types it delegates to the mutually-recursive `emit*`s
let rec private emitExpression (ctx: EmitCtx) (expr: BoundExpr) =
    let recurse = emitExpression ctx
    match expr with
    | BoundExpr.Error -> failwith "ICE: Attempt to lower an error expression"
    | BoundExpr.Null -> ctx.IL.Emit(OpCodes.Ldnull)
    | BoundExpr.Number n ->
        ctx.IL.Emit(OpCodes.Ldc_R8, n)
        ctx.IL.Emit(OpCodes.Box, ctx.Assm.MainModule.TypeSystem.Double)
    | BoundExpr.Str s -> ctx.IL.Emit(OpCodes.Ldstr, s)
    | BoundExpr.Boolean b ->
        ctx.IL.Emit(if b then OpCodes.Ldc_I4_1 else OpCodes.Ldc_I4_0)
        ctx.IL.Emit(OpCodes.Box, ctx.Assm.MainModule.TypeSystem.Boolean)
    | BoundExpr.Character c ->
        ctx.IL.Emit(OpCodes.Ldc_I4, int c)
        ctx.IL.Emit(OpCodes.Box, ctx.Assm.MainModule.TypeSystem.Char)
    | BoundExpr.Seq [] -> emitUnspecified ctx.IL
    | BoundExpr.Seq s -> emitSequence ctx s
    | BoundExpr.Application(ap, args) -> emitApplication ctx ap args
    | BoundExpr.Store(storage, maybeVal) ->
        // TODO: Could we just elide the whole definition if there is no value.
        //       If we have nothing to store it would save a lot of code. In the
        //       case we are storing to a field we _might_ need to call
        //       `emitField` still.
        match maybeVal with
        | Some(expr) -> recurse expr
        | None -> ctx.IL.Emit(OpCodes.Ldnull)
        ctx.IL.Emit(OpCodes.Dup)
        writeTo ctx storage
    | BoundExpr.Load storage -> readFrom ctx storage
    | BoundExpr.If(cond, ifTrue, maybeIfFalse) ->
        let lblTrue = ctx.IL.Create(OpCodes.Nop)
        let lblNotBool = ctx.IL.Create(OpCodes.Pop)
        let lblEnd = ctx.IL.Create(OpCodes.Nop)

        recurse cond

        ctx.IL.Emit(OpCodes.Dup)
        ctx.IL.Emit(OpCodes.Isinst, ctx.Assm.MainModule.TypeSystem.Boolean)
        ctx.IL.Emit(OpCodes.Brfalse_S, lblNotBool)

        ctx.IL.Emit(OpCodes.Unbox_Any, ctx.Assm.MainModule.TypeSystem.Boolean)
        ctx.IL.Emit(OpCodes.Brtrue, lblTrue)

        match maybeIfFalse with
        | Some ifFalse -> recurse ifFalse
        | None -> ctx.IL.Emit(OpCodes.Ldnull)
        ctx.IL.Emit(OpCodes.Br_S, lblEnd)

        ctx.IL.Append(lblNotBool)
        ctx.IL.Append(lblTrue)
        recurse ifTrue

        ctx.IL.Append(lblEnd)
    | BoundExpr.Lambda(formals, locals, captured, envSize, body) ->
        emitLambda ctx formals locals captured envSize body

    /// Emit a write to a given storage location
and writeTo ctx storage =
    match storage with
    | StorageRef.Builtin id ->
        failwithf "Can't re-define builtin %s" id
    | StorageRef.Global id ->
        let field = ensureField ctx id
        ctx.IL.Emit(OpCodes.Stsfld, field)
    | StorageRef.Local(idx) ->
        ctx.IL.Emit(OpCodes.Stloc, idx |> localToVariable ctx)
    | StorageRef.Arg(idx) ->
        ctx.IL.Emit(OpCodes.Starg, idx |> argToParam ctx)
    | StorageRef.Environment(idx, _) ->
        let temp = makeTemp ctx ctx.Assm.MainModule.TypeSystem.Object
        ctx.IL.Emit(OpCodes.Stloc, temp)
        
        ctx.IL.Emit(OpCodes.Ldloc, getEnvironment ctx)
        writeToEnv ctx temp idx
    | StorageRef.Captured(from) ->
        let temp = makeTemp ctx ctx.Assm.MainModule.TypeSystem.Object
        ctx.IL.Emit(OpCodes.Stloc, temp)

        // Start at the parent environment and walk up
        ctx.IL.Emit(OpCodes.Ldarg_0)
        walkCaptureChain ctx from (writeToEnv ctx temp)
        
    /// Emit a load from the given storage location
and readFrom ctx storage =
    match storage with
    | StorageRef.Builtin id ->
        let meth = ctx.Builtins.[id]
        emitMethodToFunc ctx meth
    | StorageRef.Global id ->
        let field = ensureField ctx id
        ctx.IL.Emit(OpCodes.Ldsfld, field)
    | StorageRef.Local(idx) ->
        ctx.IL.Emit(OpCodes.Ldloc, idx |> localToVariable ctx)
    | StorageRef.Arg(idx) ->
        ctx.IL.Emit(OpCodes.Ldarg, idx |> argToParam ctx)
    | StorageRef.Environment(idx, _) ->
        ctx.IL.Emit(OpCodes.Ldloc, getEnvironment ctx)
        readFromEnv ctx idx
    | StorageRef.Captured(from) ->
        // start at the parent environment, and walk up
        ctx.IL.Emit(OpCodes.Ldarg_0)
        walkCaptureChain ctx from (readFromEnv ctx)

    /// Emit a Sequence of Expressions
    ///
    /// Emits each expression in the sequence, and pops any intermediate values
    /// from the stack.
and emitSequence ctx seq =
    let popAndEmit x =
        ctx.IL.Emit(OpCodes.Pop)
        emitExpression ctx x
    emitExpression ctx (List.head seq)
    List.tail seq
    |>  Seq.iter popAndEmit

    /// Emit a Function application
    /// 
    /// Emits the code to build an arguments array and then call the 'applicant'
    /// value. This is responsible for 'packing' the arguments up for a call to
    /// match the 'unified calling convention' we have decided on for this
    /// Scheme compiler.
    /// 
    /// Arguments are passed to a callable instnace as an array. This means that
    /// all callable values are `Func<obj[],obj>`.
and emitApplication ctx ap args =
    emitExpression ctx ap

    // Emit the arguments array
    ctx.IL.Emit(OpCodes.Ldc_I4, List.length args)
    ctx.IL.Emit(OpCodes.Newarr, ctx.Assm.MainModule.TypeSystem.Object)
    List.fold (fun (idx: int) e -> 
        ctx.IL.Emit(OpCodes.Dup)
        ctx.IL.Emit(OpCodes.Ldc_I4, idx)
        emitExpression ctx e
        ctx.IL.Emit(OpCodes.Stelem_Ref)
        idx + 1) 0 args |> ignore
    
    let funcInvoke = typeof<Func<obj[], obj>>.GetMethod("Invoke",
                                                        [| typeof<obj[]> |])
    let funcInvoke = ctx.Assm.MainModule.ImportReference(funcInvoke)
    ctx.IL.Emit(OpCodes.Callvirt, funcInvoke)

    /// Emit a Lambda Reference
    /// 
    /// Performs the marshalling required to create a callable `Func` instance
    /// on the stack. This first defers to `emitNamedLambda` to write out the
    /// lambda's body and then creates a new `Func<obj[],obj>` instance that
    /// wraps a call to the lambda using the lambda's 'thunk'.
and emitLambda ctx formals locals captured envSize body =

    // Emit a declaration for the lambda's implementation
    let lambdaId = ctx.NextLambda
    ctx.NextLambda <- lambdaId + 1
    let _, thunk = emitNamedLambda ctx (sprintf "%s:lambda%d" ctx.ScopePrefix lambdaId) formals locals envSize body
    let method = thunk :> MethodReference

    // Create a `Func` instance with the appropriate `this` pointer.
    match ctx.EnvSize with
        | Some(_) ->
            ctx.IL.Emit(OpCodes.Ldloc, getEnvironment ctx)
            emitMethodToInstanceFunc ctx method
        | None -> emitMethodToFunc ctx method

    /// Emit a Named Lambda Body
    /// 
    /// Creates a pair of methods. The first contains the implementation of the
    /// given `body` as a true .NET method. The second method is a 'thunk'
    /// which 'unpacks' the arguments to the function and performs runtime
    /// checks as required to ensure that we match the 'unified' calling
    /// convention used by this Scheme impementation.
    /// 
    /// The thunk is rquired in this case as we can't usually know the type of
    /// the function we are calling. A future optimisation may be to transform
    /// the bound tree and lower some calls in the case we _can_ be sure of the
    /// parameters. 
and emitNamedLambda (ctx: EmitCtx) name formals localCount envMappings body =

    let attrs = 
        if ctx.EnvSize.IsSome then
            MethodAttributes.Public
        else
            MethodAttributes.Public ||| MethodAttributes.Static

    let methodDecl = MethodDefinition(name,
                                      attrs,
                                      ctx.Assm.MainModule.TypeSystem.Object)

    let mutable parameters = []
    let addParam id =
        let param = namedParam id ctx.Assm.MainModule.TypeSystem.Object
        methodDecl.Parameters.Add(param)
        parameters <- param::parameters

    // Add formals as parameter definitions
    match formals with
    | Simple id -> addParam id
    | List fmls ->
        Seq.iter addParam fmls
    | DottedList(fmls, dotted) ->
        Seq.iter addParam fmls
        addParam dotted
    
    let mutable locals = []
    for _ = 1 to localCount do
        let local = VariableDefinition(ctx.Assm.MainModule.TypeSystem.Object)
        methodDecl.Body.Variables.Add(local)
        locals <- local::locals

    // Create a new emit context for the new method, and lower the body in that
    // new context.
    let ctx = { ctx with NextLambda = 0;
                         Locals = locals;
                         Parameters = List.rev parameters;
                         EnvSize = envSizeForMappings envMappings
                         ParentEnvSize = ctx.EnvSize
                         Environment = None
                         IL = methodDecl.Body.GetILProcessor();
                         ScopePrefix = name }

    match envMappings with
    | Some m ->
        createEnvironment ctx (Option.defaultValue 0 ctx.EnvSize)
        // Hoist any arguments into the environment
        m
        |> Seq.iter (function
            | Environment(idx, Arg a) ->
                ctx.IL.Emit(OpCodes.Ldloc, getEnvironment ctx)
                ctx.IL.Emit(OpCodes.Ldfld, ctx.Core.EnvTy.Fields.[1])
                ctx.IL.Emit(OpCodes.Ldc_I4, idx)
                ctx.IL.Emit(OpCodes.Ldarg, argToParam ctx a)
                ctx.IL.Emit(OpCodes.Stelem_Ref)
            | _ -> ())
    | None -> ()

    emitExpression ctx body
    ctx.IL.Emit(OpCodes.Ret)
    methodDecl.Body.Optimize()

    // Emit a 'thunk' that unpacks the arguments to our method
    // This allows us to provide a uniform calling convention for
    // lambda instances.
    let thunkDecl = MethodDefinition((sprintf "%s:thunk" name),
                                      attrs,
                                      ctx.Assm.MainModule.TypeSystem.Object)
    thunkDecl.Parameters.Add(namedParam "args"
                                        (ArrayType(ctx.Assm.MainModule.TypeSystem.Object)))

    let thunkIl = thunkDecl.Body.GetILProcessor()

    /// Unpack a single argument from the arguments array onto the stack
    let unpackArg (idx: int) id =
        thunkIl.Emit(OpCodes.Ldarg, thunkDecl.Parameters.[0])
        thunkIl.Emit(OpCodes.Ldc_I4, idx)
        thunkIl.Emit(OpCodes.Ldelem_Ref)
        idx + 1

    /// Unpack all remaining arguments from `idx` onwards into a Scheme list
    let unpackRemainder (idx: int) =
        let i = VariableDefinition(ctx.Assm.MainModule.TypeSystem.Int32)
        thunkDecl.Body.Variables.Add(i)
        let consCtor = ctx.Core.ConsTy.GetConstructors() |> Seq.head

        // * get length of array
        thunkIl.Emit(OpCodes.Ldarg, thunkDecl.Parameters.[0])
        thunkIl.Emit(OpCodes.Ldlen)

        // * store as local <i>
        thunkIl.Emit(OpCodes.Stloc, i)
        
        // * load null
        thunkIl.Emit(OpCodes.Ldnull)

        // * First check the loop condition
        let loopCond = thunkIl.Create(OpCodes.Ldloc, i)
        thunkIl.Emit(OpCodes.Br_S, loopCond)
        
        //   * load from the array at <i> and make cons pair
        let loop = thunkIl.Create(OpCodes.Ldarg, thunkDecl.Parameters.[0])
        thunkIl.Append(loop)
        thunkIl.Emit(OpCodes.Ldloc, i)
        thunkIl.Emit(OpCodes.Ldelem_Ref)
        thunkIl.Emit(OpCodes.Newobj, consCtor)

        //   * check if <i> gt idx then loop
        thunkIl.Append(loopCond)
        thunkIl.Emit(OpCodes.Ldc_I4_1)
        thunkIl.Emit(OpCodes.Sub)
        thunkIl.Emit(OpCodes.Dup)
        thunkIl.Emit(OpCodes.Stloc, i)
        thunkIl.Emit(OpCodes.Ldc_I4, idx)
        thunkIl.Emit(OpCodes.Bge, loop)
    
    /// check the argument count using `opCode` and raise if it fails
    let raiseArgCountMismatch (count: int) opCode (err: string) =
        let ok = thunkIl.Create(OpCodes.Nop)

        thunkIl.Emit(OpCodes.Ldarg, thunkDecl.Parameters.[0])
        thunkIl.Emit(OpCodes.Ldlen)
        thunkIl.Emit(OpCodes.Ldc_I4, count)
        thunkIl.Emit(opCode, ok)

        emitThrow thunkIl ctx.Assm err

        thunkIl.Append(ok)
    
    // If we are calling an instance method we need to push `this` on the stack
    // as the first argument before we unpack any others.
    if ctx.ParentEnvSize.IsSome then
        thunkIl.Emit(OpCodes.Ldarg_0)

    match formals with
    | Simple id -> unpackRemainder 0
    | List fmls ->
        let expectedArgCount = List.length fmls
        raiseArgCountMismatch expectedArgCount OpCodes.Beq (sprintf "Expected exactly %d arguments" expectedArgCount)
        List.fold unpackArg 0 fmls |> ignore
    | DottedList(fmls, dotted) ->
        let expectedArgCount = List.length fmls
        raiseArgCountMismatch expectedArgCount OpCodes.Bge (sprintf "Expected at least %d arguments" expectedArgCount)
        let lastIdx = List.fold unpackArg 0 fmls
        unpackRemainder lastIdx

    // Call the real method as a tail call
    thunkIl.Emit(OpCodes.Tail)
    thunkIl.Emit(OpCodes.Call, methodDecl)
    thunkIl.Emit(OpCodes.Ret)

    thunkDecl.Body.Optimize()

    /// If this is method has a captures environment then add it as an instance
    /// mmethod to the environmen type. If not then add it as a plain static
    /// method to the program type.
    if ctx.ParentEnvSize.IsSome then
        ctx.Core.EnvTy.Methods.Add methodDecl
        ctx.Core.EnvTy.Methods.Add thunkDecl
    else
        ctx.ProgramTy.Methods.Add methodDecl
        ctx.ProgramTy.Methods.Add thunkDecl

    markAsCompilerGenerated thunkDecl
    methodDecl, thunkDecl

/// Emit the `Main` Method Epilogue
///
/// This sequence of instructions is added at the end of the main method to
/// coerce the result type into a return value for the application. Once we have
/// some form of runtime library linked into the final executable it might be
/// best to include this in there rather than emitting it manually each time.
let private emitMainEpilogue (assm: AssemblyDefinition) (il: ILProcessor) =
    il.Emit(OpCodes.Dup)
    il.Emit(OpCodes.Isinst, assm.MainModule.TypeSystem.Double)
    let notInt = il.Create(OpCodes.Dup)
    il.Emit(OpCodes.Brfalse, notInt)

    il.Emit(OpCodes.Unbox_Any, assm.MainModule.TypeSystem.Double)
    il.Emit(OpCodes.Conv_I4)
    il.Emit(OpCodes.Ret)

    il.Append(notInt)
    let notBool = il.Create(OpCodes.Pop)
    il.Emit(OpCodes.Isinst, assm.MainModule.TypeSystem.Boolean)
    il.Emit(OpCodes.Brfalse, notBool)
    il.Emit(OpCodes.Unbox_Any, assm.MainModule.TypeSystem.Boolean)
    let load0 = il.Create(OpCodes.Ldc_I4_0)
    il.Emit(OpCodes.Brtrue, load0)
    il.Emit(OpCodes.Ldc_I4_M1)
    il.Emit(OpCodes.Ret)

    il.Append(notBool)
    il.Append(load0)
    il.Emit(OpCodes.Ret)

/// Adds the ConsPair Type
///
/// Creates supporting types used for represnting scheme values that the
/// compiler depends on for emitted code.
let private addCoreDecls (assm: AssemblyDefinition) =
    let compilerServicesNs = "Feersum.CompilerServices"
    let consTy = TypeDefinition(compilerServicesNs,
                                "ConsPair",
                                TypeAttributes.Class ||| TypeAttributes.Public ||| TypeAttributes.AnsiClass,
                                assm.MainModule.TypeSystem.Object)
    let car = FieldDefinition("car", FieldAttributes.Public, assm.MainModule.TypeSystem.Object)
    let cdr = FieldDefinition("cdr", FieldAttributes.Public, assm.MainModule.TypeSystem.Object)

    consTy.Methods.Add <| createCtor assm (fun ctor ctorIl ->
        ctor.Parameters.Add <| namedParam "cdr" assm.MainModule.TypeSystem.Object
        ctor.Parameters.Add <| namedParam "car" assm.MainModule.TypeSystem.Object

        ctorIl.Emit(OpCodes.Ldarg_0)
        ctorIl.Emit(OpCodes.Ldarg_1)
        ctorIl.Emit(OpCodes.Stfld, cdr)

        ctorIl.Emit(OpCodes.Ldarg_0)
        ctorIl.Emit(OpCodes.Ldarg_2)
        ctorIl.Emit(OpCodes.Stfld, car))

    consTy.Fields.Add(car)
    consTy.Fields.Add(cdr)

    assm.MainModule.Types.Add consTy

    let envTy = TypeDefinition(compilerServicesNs,
                               "Environment",
                               TypeAttributes.Class ||| TypeAttributes.Public ||| TypeAttributes.AnsiClass,
                               assm.MainModule.TypeSystem.Object)
    let parent = FieldDefinition("parent", FieldAttributes.Public, envTy)
    envTy.Fields.Add(parent)
    let slots = FieldDefinition("slots", FieldAttributes.Public, ArrayType(assm.MainModule.TypeSystem.Object))
    envTy.Fields.Add(slots)

    envTy.Methods.Add <| createCtor assm (fun ctor ctorIl ->
        ctor.Parameters.Add <| namedParam "parent" envTy
        ctor.Parameters.Add <| namedParam "size" assm.MainModule.TypeSystem.Int32

        ctorIl.Emit(OpCodes.Ldarg_0)
        ctorIl.Emit(OpCodes.Ldarg_1)
        ctorIl.Emit(OpCodes.Stfld, parent)

        ctorIl.Emit(OpCodes.Ldarg_0)
        ctorIl.Emit(OpCodes.Ldarg_2)
        ctorIl.Emit(OpCodes.Newarr, assm.MainModule.TypeSystem.Object)
        ctorIl.Emit(OpCodes.Stfld, slots))
    
    assm.MainModule.Types.Add envTy

    { ConsTy = consTy; EnvTy = envTy }

/// Emit a Bound Expression to .NET
///
/// Creates an assembly and writes out the .NET interpretation of the
/// given bound tree. This method is responsible for creating the root
/// `LispProgram` type and preparting the emit context. The main work of
/// lowering is done by `emitNamedLambda`.
let emit (outputStream: Stream) outputName bound =
    // Create an assembly with a nominal version to hold our code
    let name = AssemblyNameDefinition(outputName, Version(0, 1, 0))
    let assm = AssemblyDefinition.CreateAssembly(name, "lisp_module", ModuleKind.Console)

    let coreTypes = addCoreDecls assm

    // Genreate a nominal type to contain the methods for this program.
    let progTy = TypeDefinition(outputName,
                                "LispProgram",
                                TypeAttributes.Class ||| TypeAttributes.Public ||| TypeAttributes.AnsiClass,
                                assm.MainModule.TypeSystem.Object)
    assm.MainModule.Types.Add progTy
    progTy.Methods.Add <| createEmptyCtor assm

    // Emit the body of the script to a separate method so that the `Eval`
    // module can call it directly
    let rootEmitCtx = { IL = null
                      ; ProgramTy = progTy
                      ; Core = coreTypes
                      ; Builtins = Builtins.createBuiltins assm progTy
                      ; NextLambda = 0
                      ; Locals = []
                      ; Parameters = []
                      ; EnvSize = None
                      ; ParentEnvSize = None
                      ; Environment = None
                      ; ScopePrefix = "$ROOT"
                      ; Assm = assm }
    let bodyParams = BoundFormals.List([])
    let bodyMethod, _ = emitNamedLambda rootEmitCtx "$ScriptBody" bodyParams bound.LocalsCount None bound.Root

    // The `Main` method is the entry point of the program. It calls
    // `$ScriptBody` and coerces the return value to an exit code.
    let mainMethod = MethodDefinition("Main",
                                      MethodAttributes.Public ||| MethodAttributes.Static,
                                      assm.MainModule.TypeSystem.Int32)
    mainMethod.Parameters.Add(ParameterDefinition(ArrayType(assm.MainModule.TypeSystem.String)))
    progTy.Methods.Add mainMethod
    assm.EntryPoint <- mainMethod
    let il = mainMethod.Body.GetILProcessor()

    il.Emit(OpCodes.Call, bodyMethod)
    emitMainEpilogue assm il
    markAsCompilerGenerated mainMethod

    // Write our `Assembly` to the output stream now we are done.
    assm.Write outputStream

/// Compile a single AST node into an assembly
///
/// The plan for this is we make multiple passes over the syntax tree. First
/// pass will be to `bind` theh tree. Resulting in a `BoundExpr`. This will
/// attach any type information that _can_ be computed to each node, and
/// resolve variable references to the symbols that they refer to.
///
/// Once the expression is bound we will then `emit` the expression this walks
/// the expression and writes out the corresponding .NET IL to an `Assembly`
/// at `outputStream`. The `outputName` controls the root namespace and assembly
/// name of the output.
let compile outputStream outputName node =
    let scope = createRootScope
    let bound = bind scope node
    if Diagnostics.hasErrors bound.Diagnostics then
        bound.Diagnostics
    else
        bound
        |> Lower.lower
        |> emit outputStream outputName
        []

/// Read a File and Compile
///
/// Takes the `source` to an input to read and compile. Compilation results
/// are written to `output`.
let compileFile (output: string) (source: string) =

    // Ensure the output path exists
    let outDir = Path.GetDirectoryName(output)
    if not (String.IsNullOrWhiteSpace outDir) then
        // ensure the output directory exists, no need to check it is missing first
        Directory.CreateDirectory(outDir) |> ignore
    
    // Normalise the stem and output path. This ensures the output is a file
    // not a directory.
    let stem = Path.GetFileNameWithoutExtension(output)
    let stem, output =
        if String.IsNullOrWhiteSpace(stem) then
            let stem = Path.GetFileNameWithoutExtension(source)
            stem, Path.Join(outDir, stem + ".exe")
        else
            stem, output
    
    let ast, diagnostics = parseFile source
    
    if Diagnostics.hasErrors diagnostics then
        diagnostics
    else
        let diags = compile (File.OpenWrite output) stem ast
        if diags.IsEmpty then
            // TOOD: This metadata needs to be abstracted to deal with different
            //       target framework's prefrences. For now the `.exe` we generate
            //       is compatible with .NET Core and Mono. It would be nice to make
            //       this explicit somewhere in future.
            //       It would be nice to register ourselves as a proper SDK so that
            //       this metadata is generated for us by `dotnet`.
            File.WriteAllText(Path.Combine(outDir, stem + ".runtimeconfig.json"), """
            {
              "runtimeOptions": {
                "tfm": "netcoreapp3.1",
                "framework": {
                  "name": "Microsoft.NETCore.App",
                  "version": "3.1.0"
                }
              }
            }
            """)
        diags