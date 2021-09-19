module Compile

open System
open System.IO
open Mono.Cecil
open Mono.Cecil.Rocks
open Mono.Cecil.Cil
open System.Collections.Generic
open System.Runtime.InteropServices

open Options
open Bind
open Syntax
open IlHelpers

/// Type to Hold Context While Emitting IL
type EmitCtx = 
    { Assm: AssemblyDefinition
    ; IL: ILProcessor
    ; Locals: VariableDefinition list
    ; Parameters: ParameterDefinition list
    ; DebugDocuments: Dictionary<string, Document>
    ; mutable NextLambda: int
    ; ScopePrefix: string
    ; EmitSymbols: bool
    ; Exports: Map<(string * string), string>
    ; EnvSize: int option
    ; mutable Environment: VariableDefinition option
    ; ParentEnvSize: int option
    ; ProgramTy: TypeDefinition
    ; mutable Initialisers: Map<string, MethodReference>
    ; mutable Libraries: Map<string, TypeDefinition>
    ; Core: Builtins.CoreTypes }


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

/// Mark the assembly as supporting debugging
let private markAsDebuggable (assm: AssemblyDefinition) =
    let attr =
        assm.MainModule.ImportReference(
            typeof<Diagnostics.DebuggableAttribute>
                .GetConstructor([| typeof<bool>; typeof<bool> |]))
        |> CustomAttribute

    attr.ConstructorArguments.Add(
        CustomAttributeArgument(assm.MainModule.TypeSystem.Boolean, true))
    attr.ConstructorArguments.Add(
        CustomAttributeArgument(assm.MainModule.TypeSystem.Boolean, true))

    attr
    |> assm.CustomAttributes.Add

    let attr =
        assm.MainModule.ImportReference(
            typeof<Reflection.AssemblyConfigurationAttribute>
                .GetConstructor([| typeof<string> |]))
        |> CustomAttribute
    attr.ConstructorArguments.Add(
        CustomAttributeArgument(
            assm.MainModule.TypeSystem.String,
            "Debug"))

    attr
    |> assm.CustomAttributes.Add

/// Mark the given type with a library `name`. This adds the
/// `LispLibrary` attribute to the type.
let private markWithLibraryName (typ: TypeDefinition) name =
    let attr =
        typ.Module.ImportReference(
            typeof<Serehfa.Attributes.LispLibraryAttribute>
                .GetConstructor([| typeof<string[]> |]))
        |> CustomAttribute
    attr.ConstructorArguments.Add(
        CustomAttributeArgument(
            typ.Module.TypeSystem.String.MakeArrayType(),
            name
            |> Seq.map (fun namePart ->
                CustomAttributeArgument(
                    typ.Module.TypeSystem.String,
                    namePart))
            |> Array.ofSeq))
    attr |> typ.CustomAttributes.Add

/// Mark the field as exported. This adds the `LispExport` attribute
/// to the field.
let private markWithExportedName (field: FieldDefinition) name =
    let attr =
        field.Module.ImportReference(
            typeof<Serehfa.Attributes.LispExportAttribute>
                .GetConstructor([| typeof<string> |]))
        |> CustomAttribute
    attr.ConstructorArguments.Add(
        CustomAttributeArgument(
            field.Module.TypeSystem.String,
            name))
    attr |> field.CustomAttributes.Add

/// Emit an instance of the unspecified value
let private emitUnspecified ctx =
    ctx.IL.Emit(OpCodes.Call, ctx.Core.UndefinedInstance)

/// Ensure a field exists on the program type to be used as a global variable
let private ensureField ctx mangledPrefix id =
    let pred (field: FieldDefinition) =
        field.Name = id

    let ty =
        match Map.tryFind mangledPrefix ctx.Libraries with
        | Some prefix -> prefix
        | None -> failwithf "ICE: Attempt to access field on '%s', which is not yet defined" mangledPrefix
    
    Seq.tryFind pred ty.Fields
    |> Option.map (fun f -> ctx.Assm.MainModule.ImportReference(f))
    |> Option.defaultWith (fun () ->
        let newField = FieldDefinition(id, FieldAttributes.Static, ctx.Assm.MainModule.TypeSystem.Object)
        ty.Fields.Add(newField)
        let export = Map.tryFind (mangledPrefix, id) ctx.Exports
        match export with
        | Some(exportedName) ->
            markWithExportedName newField exportedName
            newField.Attributes <- newField.Attributes ||| FieldAttributes.Public
            ()
        | None -> ()
        newField :> FieldReference)

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
let rec private emitExpression (ctx: EmitCtx) tail (expr: BoundExpr) =
    let recurse = emitExpression ctx tail
    match expr with
    | BoundExpr.Nop -> emitUnspecified ctx
    | BoundExpr.Error -> failwith "ICE: Attempt to lower an error expression"
    | BoundExpr.SequencePoint(inner, location) ->
        let pos = ctx.IL.Body.Instructions.Count
        recurse inner
        if ctx.EmitSymbols then
            let ins = ctx.IL.Body.Instructions.[pos]
            let s = location.Start
            let e = location.End
            let mutable (found, doc) = ctx.DebugDocuments.TryGetValue(s.StreamName)
            if not found then
                doc <- Document(Path.GetFullPath(s.StreamName))
                doc.Language <- DocumentLanguage.Other
                doc.LanguageGuid <- Guid("c70c3e24-e471-4637-8129-10f771417dbb")
                doc.LanguageVendor <- DocumentLanguageVendor.Other
                doc.LanguageVendorGuid <- Guid("98378869-1abf-441b-9307-3bcca9a024cd")
                ctx.DebugDocuments.[s.StreamName] <- doc
            let point = Cil.SequencePoint(ins, doc)
            point.StartLine <- int s.Line
            point.StartColumn <- int s.Column
            point.EndLine <- int e.Line
            point.EndColumn <- int e.Column
            ctx.IL.Body.Method.DebugInformation.SequencePoints.Add point
    | BoundExpr.Literal l -> emitLiteral ctx l
    | BoundExpr.Seq s -> emitSequence ctx tail s
    | BoundExpr.Application(ap, args) -> emitApplication ctx tail ap args
    | BoundExpr.Store(storage, maybeVal) ->
        // TODO: Could we just elide the whole definition if there is no value.
        //       If we have nothing to store it would save a lot of code. In the
        //       case we are storing to a field we _might_ need to call
        //       `emitField` still.
        match maybeVal with
        | Some(expr) -> emitExpression ctx false expr
        | None -> ctx.IL.Emit(OpCodes.Ldnull)
        ctx.IL.Emit(OpCodes.Dup)
        writeTo ctx storage
    | BoundExpr.Load storage -> readFrom ctx storage
    | BoundExpr.If(cond, ifTrue, maybeIfFalse) ->
        let lblTrue = ctx.IL.Create(OpCodes.Nop)
        let lblNotBool = ctx.IL.Create(OpCodes.Nop)
        let lblEnd = ctx.IL.Create(OpCodes.Nop)

        // ILAAA       : <expression for cond>
        // ILBBB       : dup
        // ILBBB       : isinst bool
        // ILBBB       : brfalse ILlblNotBool
        // ILBBB       : brtrue ILlblTrue
        // ILCCC       : <false expression>
        // ILXXX       : br ILlblEnd ; or just `ret` for tail calls
        // ILlblNotBool: pop
        // ILlblTrue   : <true expression>
        // ILlblEnd    : nop ; only if not tail call

        emitExpression ctx false cond

        ctx.IL.Emit(OpCodes.Dup)
        ctx.IL.Emit(OpCodes.Isinst, ctx.Assm.MainModule.TypeSystem.Boolean)
        ctx.IL.Emit(OpCodes.Brfalse, lblNotBool)

        ctx.IL.Emit(OpCodes.Unbox_Any, ctx.Assm.MainModule.TypeSystem.Boolean)
        ctx.IL.Emit(OpCodes.Brtrue, lblTrue)

        match maybeIfFalse with
        | Some ifFalse -> recurse ifFalse
        | None -> ctx.IL.Emit(OpCodes.Ldnull)
        if tail then
            // If we are in a tail context the JIT expects to see something like
            // the following:
            //
            // ILXZZ: tail.
            // ILXXX: callvirt Func<object[], Object>.Invoke()
            // ILXXX: ret
            //
            // WE know the target of the branch to `lblEnd` will be a `ret` so
            // cut out the middle man and just return here.
            ctx.IL.Emit(OpCodes.Ret)
        else
            ctx.IL.Emit(OpCodes.Br, lblEnd)

        ctx.IL.Append(lblNotBool)
        ctx.IL.Emit(OpCodes.Pop)
        ctx.IL.Append(lblTrue)
        recurse ifTrue

        // We only need the branch target here if we emitted a jump to it above.
        if not tail then
            // TODO: Could do with another sequence point here at the join block. Or
            //       stop using `nops` all over the place as labels and instead.
            ctx.IL.Append(lblEnd)
    | BoundExpr.Lambda(formals, body) ->
        emitLambda ctx formals body
    | BoundExpr.Library(name, mangledName, exports, body) ->
        emitLibrary ctx name mangledName exports body
    | BoundExpr.Import name ->
        match Map.tryFind name ctx.Initialisers with
        | Some(initialiser) ->
            ctx.IL.Emit(OpCodes.Call, initialiser)
        | None -> emitUnspecified ctx
    | BoundExpr.Quoted quoted ->
        emitQuoted ctx quoted

and emitQuoted ctx quoted =
    let quoteSequence s =
        let ret = makeTemp ctx ctx.Assm.MainModule.TypeSystem.Object

        // * start with null
        ctx.IL.Emit(OpCodes.Ldnull)
        ctx.IL.Emit(OpCodes.Stloc, ret)

        // * Build up list
        s
        |> List.toSeq
        |> Seq.rev
        |> Seq.iter (fun q ->
            emitQuoted ctx q
            ctx.IL.Emit(OpCodes.Ldloc, ret)
            ctx.IL.Emit(OpCodes.Newobj, ctx.Core.ConsCtor)
            ctx.IL.Emit(OpCodes.Stloc, ret))

        // * return the result
        ctx.IL.Emit(OpCodes.Ldloc, ret)

    let quoteIdent (id: string) =
        ctx.IL.Emit(OpCodes.Ldstr, id)
        ctx.IL.Emit(OpCodes.Newobj, ctx.Core.IdentCtor)

    match quoted.Kind with
    | Vector v -> emitLiteral ctx (BoundLiteral.Vector v)
    | ByteVector v -> emitLiteral ctx (BoundLiteral.ByteVector v)
    | Constant c -> emitLiteral ctx (BoundLiteral.FromConstant c)
    | Form [] -> emitLiteral ctx (BoundLiteral.Null)
    | Form s | Seq s -> quoteSequence s
    | Quoted q ->
        [ { Kind = AstNodeKind.Ident "quote"
          ; Location = quoted.Location }; q ]
        |> quoteSequence
    | Dot -> quoteIdent "."
    | Ident id -> quoteIdent id
    | Error -> failwith "Error in quoted expression"

and emitLiteral ctx = function
    | BoundLiteral.Null -> ctx.IL.Emit(OpCodes.Ldnull)
    | BoundLiteral.Number n ->
        ctx.IL.Emit(OpCodes.Ldc_R8, n)
        ctx.IL.Emit(OpCodes.Box, ctx.Assm.MainModule.TypeSystem.Double)
    | BoundLiteral.Str s -> ctx.IL.Emit(OpCodes.Ldstr, s)
    | BoundLiteral.Boolean b ->
        ctx.IL.Emit(if b then OpCodes.Ldc_I4_1 else OpCodes.Ldc_I4_0)
        ctx.IL.Emit(OpCodes.Box, ctx.Assm.MainModule.TypeSystem.Boolean)
    | BoundLiteral.Character c ->
        ctx.IL.Emit(OpCodes.Ldc_I4, int c)
        ctx.IL.Emit(OpCodes.Box, ctx.Assm.MainModule.TypeSystem.Char)
    | BoundLiteral.Vector v ->
        // Create an empty array
        ctx.IL.Emit(OpCodes.Ldc_I4, List.length v)
        ctx.IL.Emit(OpCodes.Newarr, ctx.Assm.MainModule.TypeSystem.Object)

        // Fill the array with quoted expressions
        v
        |> List.fold (fun (idx: int) e -> 
            ctx.IL.Emit(OpCodes.Dup)
            ctx.IL.Emit(OpCodes.Ldc_I4, idx)
            emitQuoted ctx e
            ctx.IL.Emit(OpCodes.Stelem_Ref)
            idx + 1) 0 |> ignore
    | BoundLiteral.ByteVector bv ->
        let len = List.length bv

        // Create an empty array
        ctx.IL.Emit(OpCodes.Ldc_I4, len)
        ctx.IL.Emit(OpCodes.Newarr, ctx.Assm.MainModule.TypeSystem.Byte)

        if len > 0 then
            // Create a static copy of the bytes
            let literalTy = TypeDefinition("Feersum.Internals",
                                           sprintf "<>ByteLiteral%d" ctx.ProgramTy.Fields.Count,
                                           TypeAttributes.NestedPrivate ||| TypeAttributes.Sealed ||| TypeAttributes.ExplicitLayout,
                                           ctx.Assm.MainModule.ImportReference(typeof<System.ValueType>))
            literalTy.PackingSize <- 1s
            literalTy.ClassSize <- len
            ctx.ProgramTy.NestedTypes.Add(literalTy)
            let literalField = FieldDefinition(sprintf "<>/Lit%d" ctx.ProgramTy.Fields.Count,
                                               FieldAttributes.Static ||| FieldAttributes.InitOnly ||| FieldAttributes.Assembly ||| FieldAttributes.HasFieldRVA,
                                               literalTy)
            literalField.InitialValue <- List.toArray bv
            ctx.ProgramTy.Fields.Add(literalField)

            /// Copy to our new array using the `InitializeArray` intrinsic.
            let initArray = ctx.Assm.MainModule.ImportReference(typeof<Runtime.CompilerServices.RuntimeHelpers>.GetMethod("InitializeArray"))
            ctx.IL.Emit(OpCodes.Dup)
            ctx.IL.Emit(OpCodes.Ldtoken, literalField)
            ctx.IL.Emit(OpCodes.Call, initArray)

    /// Emit a write to a given storage location
and writeTo ctx storage =
    match storage with
    | StorageRef.Builtin id ->
        failwithf "Can't re-define builtin %s" id
    | StorageRef.Macro m ->
        failwithf "Can't re-define macro %s" m.Name
    | StorageRef.Global(mangledPrefix, id) ->
        let field = ensureField ctx mangledPrefix id
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
    | StorageRef.Macro m ->
        failwithf "Invalid macro application %s" m.Name
    | StorageRef.Builtin id ->
        let meth = ctx.Core.Builtins.[id]
        emitMethodToFunc ctx meth
    | StorageRef.Global(mangledPrefix, id) ->
        let field = ensureField ctx mangledPrefix id
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
    /// from the stack. Emits the unspecified value if the sequence is empty.
and emitSequence ctx tail seq =
    match seq with
    | [one] ->
        emitExpression ctx tail one
    | head::rest ->
        emitExpression ctx false head
        ctx.IL.Emit(OpCodes.Pop)
        emitSequence ctx tail rest
    | _ -> emitUnspecified ctx

    /// Emit a Function application
    /// 
    /// Emits the code to build an arguments array and then call the 'applicant'
    /// value. This is responsible for 'packing' the arguments up for a call to
    /// match the 'unified calling convention' we have decided on for this
    /// Scheme compiler.
    /// 
    /// Arguments are passed to a callable instnace as an array. This means that
    /// all callable values are `Func<obj[],obj>`.
and emitApplication ctx tail ap args =
    emitExpression ctx false ap

    // Emit the arguments array
    ctx.IL.Emit(OpCodes.Ldc_I4, List.length args)
    ctx.IL.Emit(OpCodes.Newarr, ctx.Assm.MainModule.TypeSystem.Object)
    List.fold (fun (idx: int) e -> 
        ctx.IL.Emit(OpCodes.Dup)
        ctx.IL.Emit(OpCodes.Ldc_I4, idx)
        emitExpression ctx false e
        ctx.IL.Emit(OpCodes.Stelem_Ref)
        idx + 1) 0 args |> ignore
    
    let funcInvoke = typeof<Func<obj[], obj>>.GetMethod("Invoke",
                                                        [| typeof<obj[]> |])
    let funcInvoke = ctx.Assm.MainModule.ImportReference(funcInvoke)
    if tail then
        ctx.IL.Emit(OpCodes.Tail)
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
and emitNamedLambda (ctx: EmitCtx) name formals root =

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
    for _ = 1 to root.Locals do
        let local = VariableDefinition(ctx.Assm.MainModule.TypeSystem.Object)
        methodDecl.Body.Variables.Add(local)
        locals <- local::locals

    // Create a new emit context for the new method, and lower the body in that
    // new context.
    let ctx = { ctx with NextLambda = 0;
                         Locals = locals;
                         Parameters = List.rev parameters;
                         EnvSize = envSizeForMappings root.EnvMappings
                         ParentEnvSize = ctx.EnvSize
                         Environment = None
                         IL = methodDecl.Body.GetILProcessor();
                         ScopePrefix = name }

    match root.EnvMappings with
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

    emitExpression ctx true root.Body
    ctx.IL.Emit(OpCodes.Ret)
    methodDecl.Body.Optimize()

    if ctx.EmitSymbols then
        let scope =
            ScopeDebugInformation(
                methodDecl.Body.Instructions.[0],
                methodDecl.Body.Instructions.[methodDecl.Body.Instructions.Count - 1])

        // If we have an environment tell the debugger about it
        ctx.Environment
        |> Option.iter (fun env ->
            VariableDebugInformation(env, "capture-environment")
            |> scope.Variables.Add)

        ctx.Locals
        |> List.iteri (fun idx var ->
            VariableDebugInformation(var, sprintf "local%d" idx)
            |> scope.Variables.Add)

        methodDecl.DebugInformation.Scope <- scope

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
        let ret = VariableDefinition(ctx.Assm.MainModule.TypeSystem.Object)
        thunkDecl.Body.Variables.Add(ret)

        // * get length of array
        thunkIl.Emit(OpCodes.Ldarg, thunkDecl.Parameters.[0])
        thunkIl.Emit(OpCodes.Ldlen)

        // * store as local <i>
        thunkIl.Emit(OpCodes.Stloc, i)
        
        // * load null
        thunkIl.Emit(OpCodes.Ldnull)
        thunkIl.Emit(OpCodes.Stloc, ret)

        // * First check the loop condition
        let loopCond = thunkIl.Create(OpCodes.Ldloc, i)
        thunkIl.Emit(OpCodes.Br, loopCond)
        
        //   * load from the array at <i> and make cons pair
        let loop = thunkIl.Create(OpCodes.Ldarg, thunkDecl.Parameters.[0])
        thunkIl.Append(loop)
        thunkIl.Emit(OpCodes.Ldloc, i)
        thunkIl.Emit(OpCodes.Ldelem_Ref)
        thunkIl.Emit(OpCodes.Ldloc, ret)
        thunkIl.Emit(OpCodes.Newobj, ctx.Core.ConsCtor)
        thunkIl.Emit(OpCodes.Stloc, ret)

        //   * check if <i> gt idx then loop
        thunkIl.Append(loopCond)
        thunkIl.Emit(OpCodes.Ldc_I4_1)
        thunkIl.Emit(OpCodes.Sub)
        thunkIl.Emit(OpCodes.Dup)
        thunkIl.Emit(OpCodes.Stloc, i)
        thunkIl.Emit(OpCodes.Ldc_I4, idx)
        thunkIl.Emit(OpCodes.Bge, loop)

        //   * load the result
        thunkIl.Emit(OpCodes.Ldloc, ret)
    
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

/// Emit the body of a library definition
and emitLibrary ctx name mangledName exports body =

    // Genreate a nominal type to contain the methods for this library.
    let libTy = TypeDefinition(ctx.ProgramTy.Namespace,
                                mangledName,
                                TypeAttributes.Class ||| TypeAttributes.Public ||| TypeAttributes.AnsiClass,
                                ctx.Assm.MainModule.TypeSystem.Object)
    ctx.Assm.MainModule.Types.Add libTy
    libTy.Methods.Add <| createEmptyCtor ctx.Assm
    markWithLibraryName libTy name
    ctx.Libraries <- Map.add mangledName libTy ctx.Libraries

    let exports =
        exports
        |> Seq.choose (fun (name, storage) ->
            match storage with
            | StorageRef.Global(mangled, id) -> Some(((mangled, id), name))
            | _ -> None)
        |> Map.ofSeq

    // Emit the body of the script to a separate method so that the `Eval`
    // module can call it directly
    let libEmitCtx  = { ctx with IL = null
                              ; ProgramTy = libTy
                              ; NextLambda = 0
                              ; Locals = []
                              ; Parameters = []
                              ; Exports = exports
                              ; EnvSize = None
                              ; ParentEnvSize = None
                              ; Environment = None
                              ; ScopePrefix = "$ROOT" }
    let bodyParams = BoundFormals.List([])
    let bodyMethod, _ = emitNamedLambda libEmitCtx "$LibraryBody" bodyParams body

    ctx.Initialisers <- Map.add mangledName (bodyMethod :> MethodReference) ctx.Initialisers

    emitUnspecified ctx

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

/// Emit a Bound Expression to .NET
///
/// Creates an assembly and writes out the .NET interpretation of the
/// given bound tree. This method is responsible for creating the root
/// `LispProgram` type and preparting the emit context. The main work of
/// lowering is done by `emitNamedLambda`.
let emit options (outputStream: Stream) outputName (symbolStream: Stream option) refLibs bound =

    /// Collect external types so we can look up their fields later
    let externs =
        refLibs
        |> Seq.map (fun ((ty: TypeDefinition), _) ->
            (ty.FullName, ty))
        |> Map.ofSeq

    // Create an assembly with a nominal version to hold our code
    let name = AssemblyNameDefinition(outputName, Version(0, 1, 0))
    let assm = AssemblyDefinition.CreateAssembly(name, "lisp_module", ModuleKind.Console)

    /// Import the initialisers for the extern libraries
    let inits =
        refLibs
        |> Seq.map (fun ((ty: TypeDefinition), _) ->
            (ty.Name
            , (Seq.find (fun (m: MethodDefinition) -> m.Name = "$LibraryBody") ty.Methods)
               |> assm.MainModule.ImportReference))
        |> Map.ofSeq

    if symbolStream.IsSome then
        markAsDebuggable assm

    // Genreate a nominal type to contain the methods for this program.
    let progTy = TypeDefinition(outputName,
                                bound.MangledName,
                                TypeAttributes.Class ||| TypeAttributes.Public ||| TypeAttributes.AnsiClass,
                                assm.MainModule.TypeSystem.Object)
    assm.MainModule.Types.Add progTy
    progTy.Methods.Add <| createEmptyCtor assm

    let coreTypes = Builtins.importCore assm

    // Emit the body of the script to a separate method so that the `Eval`
    // module can call it directly
    let rootEmitCtx = { IL = null
                      ; DebugDocuments = Dictionary()
                      ; ProgramTy = progTy
                      ; Libraries = Map.add bound.MangledName progTy externs
                      ; Initialisers = inits
                      ; Core = coreTypes
                      ; NextLambda = 0
                      ; Locals = []
                      ; Exports = Map.empty
                      ; Parameters = []
                      ; EmitSymbols = symbolStream.IsSome
                      ; EnvSize = None
                      ; ParentEnvSize = None
                      ; Environment = None
                      ; ScopePrefix = "$ROOT"
                      ; Assm = assm }
    let bodyParams = BoundFormals.List([])
    let bodyMethod, _ = emitNamedLambda rootEmitCtx "$ScriptBody" bodyParams bound.Root

    if options.OutputType = OutputType.Exe then
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
    let mutable writerParams = WriterParameters()
    match symbolStream with
    | Some(stream) ->
        writerParams.SymbolStream <- stream
        writerParams.WriteSymbols <- true
        writerParams.SymbolWriterProvider <- PortablePdbWriterProvider()
    | None -> ()
    assm.Write(outputStream, writerParams)

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
let compile options outputStream outputName symbolStream node =
    let coreLibs = Builtins.loadCoreSignatures
    let refLibs =
        options.References
        |> List.collect (Builtins.loadReferencedSignatures)
    let refSigs = List.map (fun (_, s) -> s) refLibs
    let allLibs = (List.append coreLibs refSigs)
    let scope =
        if options.OutputType = OutputType.Script then
            scopeFromLibraries allLibs
        else
            emptyScope
    let bound = bind scope allLibs node
    if Diagnostics.hasErrors bound.Diagnostics |> not then
        bound
        |> Lower.lower
        |> emit options outputStream outputName symbolStream refLibs
    bound.Diagnostics

/// Read a File and Compile
///
/// Takes the `source` to an input to read and compile. Compilation results
/// are written to `output`.
let compileFile options (output: string) (source: string) =

    // Handle the case that the user has speciied a path to a directory but
    // is missing the trailing `/`
    let outDir, output =
        if Directory.Exists(output) then
            if Path.EndsInDirectorySeparator(output) then
                output, output
            else
                output, sprintf "%s%c" output Path.DirectorySeparatorChar
        else
            // Ensure the output path exists
            let dir = Path.GetDirectoryName(output)
            if not (String.IsNullOrWhiteSpace dir) then
                // ensure the output directory exists, no need to check it is missing first
                Directory.CreateDirectory(dir) |> ignore
            dir, output

    // Normalise the stem and output path. This ensures the output is a file
    // not a directory.
    let stem = Path.GetFileNameWithoutExtension(output)
    let stem, output =
        if String.IsNullOrWhiteSpace(stem) then
            let stem = Path.GetFileNameWithoutExtension(source)
            stem, Path.Join(outDir, getDefaultExtension options |> sprintf "%s.%s" stem)
        else
            stem, output
    
    let ast, diagnostics = parseFile source
    
    if Diagnostics.hasErrors diagnostics then
        diagnostics
    else

        // Open the output streams. We don't use an `Option` directly here for
        // the symbols stream so we can drop it with `use`.
        use outputStream = File.OpenWrite output
        use symbols =
            match options.Configuration with
            | BuildConfiguration.Debug ->
                File.OpenWrite(Path.ChangeExtension(output, "pdb")) :> Stream
                // make a symbol file
            | BuildConfiguration.Release -> null

        let diags = compile options outputStream stem (symbols |> Option.ofObj) ast

        if diags.IsEmpty && options.OutputType = OutputType.Exe then
            // TOOD: This metadata needs to be abstracted to deal with different
            //       target framework's prefrences. For now the `.exe` we generate
            //       is compatible with .NET Core and Mono. It would be nice to make
            //       this explicit somewhere in future.
            //       It would be nice to register ourselves as a proper SDK so that
            //       this metadata is generated for us by `dotnet`.
            let tfmPrefix =
                if RuntimeInformation.FrameworkDescription.StartsWith ".NET Core" then
                    "netcoreapp"
                else
                    "net"
            let tfVersion = Environment.Version
            let config =
                sprintf """
                {
                  "runtimeOptions": {
                    "tfm": "%s%i.%i",
                    "framework": {
                      "name": "Microsoft.NETCore.App",
                      "version": "%A"
                    }
                  }
                }
                """ tfmPrefix tfVersion.Major tfVersion.Minor tfVersion
            File.WriteAllText(Path.Combine(outDir, stem + ".runtimeconfig.json"), config)
            // FIXME: Copying the core assembly like this is a bit of a hack.
            let corePath = typeof<Serehfa.ConsPair>.Assembly.Location
            File.Copy(corePath, Path.Join(outDir, Path.GetFileName(corePath)), true)
        diags
