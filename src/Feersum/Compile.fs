module Compile

open Bind
open Syntax
open System.IO
open System
open Mono.Cecil
open Mono.Cecil.Rocks
open Mono.Cecil.Cil

// Type to Hold Context While Emitting IL
type EmitCtx =
    { Assm: AssemblyDefinition
    ; IL: ILProcessor
    ; mutable NextLambda: int
    ; ScopePrefix: string
    ; ProgramTy: TypeDefinition
    ; Builtins: Map<string,MethodDefinition>
    ; ConsTy: TypeDefinition }

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

/// Emit a sequence of instructions to convert a method reference
/// into a `Func<obj[],obj>`
let private emitMethodToFunc ctx (method: MethodReference) =
    let paramTypes = [|typeof<obj>; typeof<IntPtr>|]
    let funcObjCtor = typeof<Func<obj[], obj>>.GetConstructor(paramTypes)
    let funcObjCtor = ctx.Assm.MainModule.ImportReference(funcObjCtor)
    ctx.IL.Emit(OpCodes.Ldnull)
    ctx.IL.Emit(OpCodes.Ldftn, method)
    ctx.IL.Emit(OpCodes.Newobj, funcObjCtor)

/// Emit a Single Bound Expression
///
/// Emits the code for a single function into the given assembly. For some more
/// complex expression types it delegates to the mutually-recursive `emit*`s
let rec private emitExpression (ctx: EmitCtx) (expr: BoundExpr) =
    let recurse = emitExpression ctx
    match expr with
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
    | BoundExpr.Definition(id, storage, maybeVal) ->
        // TODO: Could we just elide the whole definition if there is no value.
        //       If we have nothing to store it would save a lot of code. In the
        //       case we are storing to a field we _might_ need to call
        //       `emitField` still.
        match maybeVal with
        | Some(expr) -> recurse expr
        | None -> ctx.IL.Emit(OpCodes.Ldnull)
        ctx.IL.Emit(OpCodes.Dup)
        match storage with
        | StorageRef.Builtin id ->
            failwithf "Can't re-define builtin %s" id
        | StorageRef.Global id ->
            let field = ensureField ctx id
            ctx.IL.Emit(OpCodes.Stsfld, field)
        | StorageRef.Local(idx) ->
            ctx.IL.Emit(OpCodes.Stloc, idx)
        | StorageRef.Arg(idx) ->
            ctx.IL.Emit(OpCodes.Starg, idx)
    | BoundExpr.Load storage ->
        match storage with
        | StorageRef.Builtin id ->
            let meth = ctx.Builtins.[id]
            emitMethodToFunc ctx meth
        | StorageRef.Global id ->
            let field = ensureField ctx id
            ctx.IL.Emit(OpCodes.Ldsfld, field)
        | StorageRef.Local(idx) ->
            ctx.IL.Emit(OpCodes.Ldloc, idx)
        | StorageRef.Arg(idx) ->
            ctx.IL.Emit(OpCodes.Ldarg, idx)
    | BoundExpr.If(cond, ifTrue, maybeIfFalse) ->
        recurse cond
        let lblFalse = ctx.IL.Create(OpCodes.Nop)
        let lblEnd = ctx.IL.Create(OpCodes.Nop)
        ctx.IL.Emit(OpCodes.Brfalse_S, lblFalse)
        recurse ifTrue
        ctx.IL.Emit(OpCodes.Br_S, lblEnd)
        ctx.IL.Append(lblFalse)
        match maybeIfFalse with
        | Some ifFalse -> recurse ifFalse
        | None -> ctx.IL.Emit(OpCodes.Ldnull)
        ctx.IL.Append(lblEnd)
    | BoundExpr.Lambda(formals, body) ->
        emitLambda ctx formals body

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
and emitLambda ctx formals body =
    // Emit a declaration for the lambda's implementation
    let lambdaId = ctx.NextLambda
    ctx.NextLambda <- lambdaId + 1
    let _, thunk = emitNamedLambda ctx (sprintf "%s:lambda%d" ctx.ScopePrefix lambdaId) formals body
    let method = thunk :> MethodReference
    
    emitMethodToFunc ctx method

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
and emitNamedLambda (ctx: EmitCtx) name formals body =
    let methodDecl = MethodDefinition(name,
                                      MethodAttributes.Public ||| MethodAttributes.Static,
                                      ctx.Assm.MainModule.TypeSystem.Object)
    ctx.ProgramTy.Methods.Add methodDecl

    let addParam id =
        let param = ParameterDefinition(id,
                                        ParameterAttributes.None,
                                        ctx.Assm.MainModule.TypeSystem.Object)
        methodDecl.Parameters.Add(param)

    // Add formals as parameter definitions
    match formals with
    | Simple id -> addParam id
    | List fmls ->
        List.map addParam fmls |> ignore
    | DottedList(fmls, dotted) ->
        List.map addParam fmls |> ignore
        addParam dotted

    // Create a new emit context for the new method, and lower the body in that
    // new context.
    let ctx = { ctx with NextLambda = 0;
                         IL = methodDecl.Body.GetILProcessor();
                         ScopePrefix = name }
    emitExpression ctx body
    ctx.IL.Emit(OpCodes.Ret)
    methodDecl.Body.Optimize()

    // Emit a 'thunk' that unpacks the arguments to our method
    // This allows us to provide a uniform calling convention for
    // lambda instances.
    let thunkDecl = MethodDefinition((sprintf "%s:thunk" name),
                                      MethodAttributes.Public ||| MethodAttributes.Static,
                                      ctx.Assm.MainModule.TypeSystem.Object)
    thunkDecl.Parameters.Add(ParameterDefinition(ArrayType(ctx.Assm.MainModule.TypeSystem.Object)))
    ctx.ProgramTy.Methods.Add thunkDecl
    let thunkIl = thunkDecl.Body.GetILProcessor()

    /// Unpack a single argument from the arguments array onto the stack
    let unpackArg (idx: int) id =
        thunkIl.Emit(OpCodes.Ldarg_0)
        thunkIl.Emit(OpCodes.Ldc_I4, idx)
        thunkIl.Emit(OpCodes.Ldelem_Ref)
        idx + 1

    /// Unpack all remaining arguments from `idx` onwards into a Scheme list
    let unpackRemainder (idx: int) =
        let i = VariableDefinition(ctx.Assm.MainModule.TypeSystem.Int32)
        thunkDecl.Body.Variables.Add(i)
        let consCtor = ctx.ConsTy.GetConstructors() |> Seq.head

        // * get length of array
        thunkIl.Emit(OpCodes.Ldarg_0)
        thunkIl.Emit(OpCodes.Ldlen)

        // * store as local <i>
        thunkIl.Emit(OpCodes.Stloc, i)
        
        // * load null
        thunkIl.Emit(OpCodes.Ldnull)

        // * First check the loop condition
        let loopCond = thunkIl.Create(OpCodes.Ldloc, i)
        thunkIl.Emit(OpCodes.Br_S, loopCond)
        
        //   * load from the array at <i> and make cons pair
        let loop = thunkIl.Create(OpCodes.Ldarg_0)
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

        thunkIl.Emit(OpCodes.Ldarg_0)
        thunkIl.Emit(OpCodes.Ldlen)
        thunkIl.Emit(OpCodes.Ldc_I4, count)
        thunkIl.Emit(opCode, ok)

        thunkIl.Emit(OpCodes.Ldstr, err)
        thunkIl.Emit(OpCodes.Newobj, ctx.Assm.MainModule.ImportReference(typeof<Exception>.GetConstructor([| typeof<string> |])))
        thunkIl.Emit(OpCodes.Throw)

        thunkIl.Append(ok)

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

/// Create an Empty Object Constructor
///
/// Creates a constructor method deifnition that just calls the parent constructor
let private createEmptyCtor (assm: AssemblyDefinition) =
    let ctor = MethodDefinition(".ctor",
                                MethodAttributes.Public |||
                                MethodAttributes.HideBySig |||
                                MethodAttributes.SpecialName |||
                                MethodAttributes.RTSpecialName,
                                assm.MainModule.TypeSystem.Void)
    let objConstructor = assm.MainModule.ImportReference(typeof<obj>.GetConstructor(Array.empty))
    let il = ctor.Body.GetILProcessor()
    il.Emit(OpCodes.Ldarg_0)
    il.Emit(OpCodes.Call, objConstructor)
    il.Emit(OpCodes.Ret)
    ctor

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

    let ctor = MethodDefinition(".ctor",
                                MethodAttributes.Public |||
                                MethodAttributes.HideBySig |||
                                MethodAttributes.SpecialName |||
                                MethodAttributes.RTSpecialName,
                                assm.MainModule.TypeSystem.Void)
    ctor.Parameters.Add <| ParameterDefinition(assm.MainModule.TypeSystem.Object)
    ctor.Parameters.Add <| ParameterDefinition(assm.MainModule.TypeSystem.Object)

    let objConstructor = assm.MainModule.ImportReference(typeof<obj>.GetConstructor(Array.empty))
    let ctorIl = ctor.Body.GetILProcessor()
    ctorIl.Emit(OpCodes.Ldarg_0)
    ctorIl.Emit(OpCodes.Call, objConstructor)
    
    ctorIl.Emit(OpCodes.Ldarg_0)
    ctorIl.Emit(OpCodes.Ldarg_1)
    ctorIl.Emit(OpCodes.Stfld, cdr)

    ctorIl.Emit(OpCodes.Ldarg_0)
    ctorIl.Emit(OpCodes.Ldarg_2)
    ctorIl.Emit(OpCodes.Stfld, car)

    ctorIl.Emit(OpCodes.Ret)
        
    consTy.Methods.Add ctor

    consTy.Fields.Add(car)
    consTy.Fields.Add(cdr)

    assm.MainModule.Types.Add consTy
    consTy

let createBuiltins (assm: AssemblyDefinition) (ty: TypeDefinition) =
    let declareBuiltinMethod name =
        let meth = MethodDefinition(name,
                                MethodAttributes.Public ||| MethodAttributes.Static,
                                assm.MainModule.TypeSystem.Object)
        let args = ParameterDefinition(ArrayType(assm.MainModule.TypeSystem.Object))
        meth.Parameters.Add(args)
        ty.Methods.Add meth
        let il = meth.Body.GetILProcessor()
        (meth, il)

    let createArithBuiltin name opcode (def: double) = 
        let meth, il = declareBuiltinMethod name

        let i = VariableDefinition(assm.MainModule.TypeSystem.Int32)
        meth.Body.Variables.Add(i)


        let setup = il.Create(OpCodes.Nop)
        let cond = il.Create(OpCodes.Nop)

        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldlen)
        il.Emit(OpCodes.Ldc_I4_1)
        il.Emit(OpCodes.Ble, setup)

        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Ldelem_Ref)
        il.Emit(OpCodes.Unbox_Any, assm.MainModule.TypeSystem.Double)
        il.Emit(OpCodes.Ldc_I4_1)
        il.Emit(OpCodes.Stloc, i)
        il.Emit(OpCodes.Br, cond)

        il.Append(setup)

        il.Emit(OpCodes.Ldc_R8, def)
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Stloc, i)
        il.Emit(OpCodes.Br, cond)

        let loop = il.Create(OpCodes.Nop)
        il.Append(loop)

        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldloc, i)
        il.Emit(OpCodes.Ldelem_Ref)

        il.Emit(OpCodes.Unbox_Any, assm.MainModule.TypeSystem.Double)
        il.Emit(opcode)

        il.Emit(OpCodes.Ldloc, i)
        il.Emit(OpCodes.Ldc_I4_1)
        il.Emit(OpCodes.Add)
        il.Emit(OpCodes.Stloc, i)

        il.Append(cond)
        il.Emit(OpCodes.Ldloc, i)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldlen)
        il.Emit(OpCodes.Blt, loop)

        il.Emit(OpCodes.Box, assm.MainModule.TypeSystem.Double)
        il.Emit(OpCodes.Ret)

        meth
    
    let createCompBuiltin name op =
        let meth, il = declareBuiltinMethod name

        let last = VariableDefinition(assm.MainModule.TypeSystem.Double)
        meth.Body.Variables.Add(last)
        let i = VariableDefinition(assm.MainModule.TypeSystem.Int32)
        meth.Body.Variables.Add(i)

        let main = il.Create(OpCodes.Nop)

        // If we have only 0 or 1 arguments just return true
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldlen)
        il.Emit(OpCodes.Ldc_I4_1)
        il.Emit(OpCodes.Bgt, main)
        il.Emit(OpCodes.Ldc_I4_1)
        il.Emit(OpCodes.Box, assm.MainModule.TypeSystem.Boolean)
        il.Emit(OpCodes.Ret)

        il.Append(main)

        // Load the first element and store it as `last` then begin the loop
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Ldelem_Ref)
        il.Emit(OpCodes.Unbox_Any, assm.MainModule.TypeSystem.Double)
        il.Emit(OpCodes.Stloc, last)

        // start at index i
        il.Emit(OpCodes.Ldc_I4_1)
        il.Emit(OpCodes.Stloc, i)
        
        let loop = il.Create(OpCodes.Nop)
        il.Append(loop)

        // Get the last + current from the array
        il.Emit(OpCodes.Ldloc, last)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldloc, i)
        il.Emit(OpCodes.Ldelem_Ref)
        il.Emit(OpCodes.Unbox_Any, assm.MainModule.TypeSystem.Double)
        il.Emit(OpCodes.Dup)
        il.Emit(OpCodes.Stloc, last)

        let cond = il.Create(OpCodes.Nop)
        
        il.Emit(op, cond)

        // Check failed, so return `#f`
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Box, assm.MainModule.TypeSystem.Boolean)
        il.Emit(OpCodes.Ret)

        il.Append(cond)

        // check if i works for rest of loop
        il.Emit(OpCodes.Ldloc, i)
        il.Emit(OpCodes.Ldc_I4_1)
        il.Emit(OpCodes.Add)
        il.Emit(OpCodes.Dup)
        il.Emit(OpCodes.Stloc, i)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldlen)
        il.Emit(OpCodes.Blt, loop)

        // Ran out of things to check, return `#t`
        il.Emit(OpCodes.Ldc_I4_1)
        il.Emit(OpCodes.Box, assm.MainModule.TypeSystem.Boolean)
        il.Emit(OpCodes.Ret)

        meth

    [ ("+", createArithBuiltin "+" OpCodes.Add 0.0)
    ; ("-", createArithBuiltin "-" OpCodes.Sub 0.0)
    ; ("/", createArithBuiltin "/" OpCodes.Div 1.0)
    ; ("*", createArithBuiltin "*" OpCodes.Mul 1.0)
    ; ("=", createCompBuiltin "aritheq" OpCodes.Beq)
    ; (">", createCompBuiltin "arithgt" OpCodes.Bgt)
    ; ("<", createCompBuiltin "arithlt" OpCodes.Blt)
    ; (">=", createCompBuiltin "arithgte" OpCodes.Bge)
    ; ("<=", createCompBuiltin "arithlte" OpCodes.Ble) ]
    |> Map.ofSeq

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

    let consTy = addCoreDecls assm

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
                      ; ConsTy = consTy
                      ; Builtins = createBuiltins assm progTy
                      ; NextLambda = 0
                      ; ScopePrefix = "$ROOT"
                      ; Assm = assm }
    let bodyParams = BoundFormals.List([])
    let bodyMethod, _ = emitNamedLambda rootEmitCtx "$ScriptBody" bodyParams bound

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
    bind scope node |> emit outputStream outputName

/// Read a File and Compile
///
/// Takes the `path` to an input to read and compile.
let compileFile (path: string) =
    let output = Path.ChangeExtension(path, "exe")
    let stem = Path.GetFileNameWithoutExtension(path);
    parseFile path
    |> Result.map (fun ast ->
        compile (File.OpenWrite output) stem ast
        // TOOD: This metadata needs to be abstracted to deal with different
        //       target framework's prefrences. For now the `.exe` we generate
        //       is compatible with .NET Core and Mono. It would be nice to make
        //       this explicit somewhere in future.
        //       It would be nice to register ourselves as a proper SDK so that
        //       this metadata is generated for us by `dotnet`.
        File.WriteAllText(Path.Combine(Path.GetDirectoryName(path), stem + ".runtimeconfig.json"), """
        {
          "runtimeOptions": {
            "tfm": "netcoreapp3.0",
            "framework": {
              "name": "Microsoft.NETCore.App",
              "version": "3.0.0"
            }
          }
        }
        """))