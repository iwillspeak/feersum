module Builtins

open System.Reflection
open Mono.Cecil
open Mono.Cecil.Rocks
open Mono.Cecil.Cil

open IlHelpers
open Macros
open System
open Syntax
open Utils
open Bind
open Libraries

/// Core Types Used by the Compiler at Runtime
type CoreTypes =
    { ConsTy: TypeReference
    ; ValueType: TypeReference
    ; EnvTy: TypeDefinition
    ; ConsCtor: MethodReference
    ; UndefinedInstance: MethodReference
    ; IdentCtor: MethodReference
    ; RuntimeInitArray: MethodReference
    ; FuncObjCtor: MethodReference
    ; FuncObjInvoke: MethodReference
    ; ExceptionCtor: MethodReference
    ; DebuggableCtor: MethodReference
    ; CompGenCtor: MethodReference
    ; NonUserCodeCtor: MethodReference
    ; StepThroughCtor: MethodReference
    ; LispExport: MethodReference
    ; LispLibrary: MethodReference
    ; AssmConfigCtor: MethodReference
    ; Builtins: Map<string * string, MethodReference> }

/// Reder parameters for Mono assembly loading. 
let private assmReadParams =
    let r = ReaderParameters()
    r.ReadSymbols <- false
    r

/// Adds the Environment Type
///
/// Creates the environment class that is used to hold dynamic environments
/// introduced by lambda captures..
let private addEnvDecls (assm: AssemblyDefinition) =
    let compilerServicesNs = "Feersum.CompilerServices"

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

    envTy

/// Map the exports of a given type using `onGlobal` for `LispExport`s and
/// `onBuiltin` for `LispBuiltin`s.
let private cataExports onGlobal onBuiltin (ty: TypeDefinition) =

    let unpackStringArg (attr: CustomAttribute) =
        attr.ConstructorArguments.[0].Value.ToString()

    let chooseMatching name onMatching (things: seq<'a> when 'a:> ICustomAttributeProvider) =
        things
        |> Seq.choose (fun thing ->
            thing.CustomAttributes
            |> Seq.tryPick (fun attr ->
                if attr.AttributeType.Name = name then
                    Some(onMatching (unpackStringArg attr) thing)
                else
                    None))

    let exports =
        ty.Fields |> chooseMatching "LispExportAttribute" (onGlobal ty.FullName)
    let builtins =
        ty.Methods |> chooseMatching "LispBuiltinAttribute" (onBuiltin ty.FullName)

    Seq.append exports builtins

/// Get Exported items from a given Mono type definition.
let private getExports =
    cataExports (fun ty name field -> (name, Global(ty, Field field.Name))) (fun ty name _ -> (name, Global(ty, Method name)))
    >> List.ofSeq

/// Maybe map a given type if it has a `LispLibrary` name. 
let private tryCataType onLib (ty: TypeDefinition) =
    ty.CustomAttributes
    |> Seq.tryPick (fun attr ->
        if attr.AttributeType.Name = "LispLibraryAttribute" then
            Some(attr.ConstructorArguments.[0].Value :?> CustomAttributeArgument[])
        else
            None)
    |> Option.map (onLib ty)

/// Try to convert a given type definition into a library signature.
let private tryGetSignatureFromType =
    tryCataType (fun ty name ->
        (ty, { LibraryName = name |> Seq.map (fun a -> a.Value.ToString()) |> List.ofSeq
             ; Exports = getExports ty }))

/// Import method references for any builtins in the `externAssm`
let private loadExternBuiltins (lispAssm: AssemblyDefinition) (externAssm: AssemblyDefinition) =
    let onTy ty _ =
        ty
        |> cataExports (fun _ _ _ -> None) (fun ty id method -> Some(((ty, id), method)))
        |> Seq.choose id
    externAssm.MainModule.Types
    |> Seq.choose (tryCataType onTy)
    |> Seq.concat
    |> Seq.map (fun (name, m) -> (name, lispAssm.MainModule.ImportReference(m :> MethodReference)))
    |> Map.ofSeq

/// Convert a method reference on a generic type to a method reference on a bound
/// generic instance type.
/// 
/// https://stackoverflow.com/a/16433452/1353098   - CC BY-SA 4.0
let private makeHostInstanceGeneric args (method: MethodReference) =
    let reference = 
        MethodReference(
            method.Name,
            method.ReturnType,
            method.DeclaringType.MakeGenericInstanceType(args)
        )
    reference.HasThis <- method.HasThis
    reference.ExplicitThis <- method.ExplicitThis
    reference.CallingConvention <- method.CallingConvention

    method.Parameters
    |> Seq.iter (fun parameter ->
        reference.Parameters.Add(ParameterDefinition(parameter.ParameterType)))

    method.GenericParameters
    |> Seq.iter (fun genericParam ->
        reference.GenericParameters.Add(GenericParameter(genericParam.Name, reference)))

    reference

/// Scan the `externAssms` and retrieve the core types that are required to
/// compile a scheme progrma. These `CoreTypes` represent the types and methods
/// that the compilation will emit calls to as part of code generation and
/// intrisics.
let private loadCoreTypes (lispAssm: AssemblyDefinition) (externAssms: seq<AssemblyDefinition>) =

    let getType name =
        externAssms
        |> Seq.pick (fun (assm: AssemblyDefinition) ->
            assm.MainModule.Types
            |> Seq.tryFind (fun x  -> x.FullName = name))
        |> lispAssm.MainModule.ImportReference

    let getResolvedType name =
        (getType name).Resolve()

    let getCtorBy pred typeName =
        let ty = getResolvedType typeName
        ty.GetConstructors()
        |> Seq.find pred
        |> lispAssm.MainModule.ImportReference
    let getSingleCtor = getCtorBy (fun _ -> true)
    let getCtorByArity arity = getCtorBy (fun m -> m.Parameters.Count = arity)

    // Func<object[], object> is akward because we have to convert both the
    // constructor and invoke methods into instnace methods on the correctly
    // bound generic instance.
    let objTy = lispAssm.MainModule.TypeSystem.Object
    let genericArgs = [| objTy.MakeArrayType() :> TypeReference; objTy |]
    let funcTy = (getType "System.Func`2").MakeGenericInstanceType(genericArgs).Resolve()
    let funcCtor =
        lispAssm.MainModule.ImportReference(
            funcTy.GetConstructors() |> Seq.head
        ) |> makeHostInstanceGeneric genericArgs
    let funcInvoke =
        lispAssm.MainModule.ImportReference(
            funcTy.GetMethods() |> Seq.find (fun m -> m.Name = "Invoke")
        ) |> makeHostInstanceGeneric genericArgs

    let getMethod typeName methodName =
        let ty = getResolvedType typeName
        ty.GetMethods()
        |> Seq.find (fun m -> m.Name = methodName)
        |> lispAssm.MainModule.ImportReference

    { ConsTy = getType "Serehfa.ConsPair"
    ; ValueType = getType "System.ValueType"
    ; ConsCtor = getSingleCtor "Serehfa.ConsPair"
    ; IdentCtor = getSingleCtor "Serehfa.Ident"
    ; RuntimeInitArray = getMethod "System.Runtime.CompilerServices.RuntimeHelpers" "InitializeArray"
    ; UndefinedInstance = getMethod "Serehfa.Undefined" "get_Instance"
    ; FuncObjCtor = funcCtor
    ; FuncObjInvoke = funcInvoke
    ; ExceptionCtor = getCtorByArity 1 "System.Exception"
    ; DebuggableCtor = getCtorByArity 2 "System.Diagnostics.DebuggableAttribute"
    ; CompGenCtor = getSingleCtor "System.Runtime.CompilerServices.CompilerGeneratedAttribute"
    ; NonUserCodeCtor = getSingleCtor "System.Diagnostics.DebuggerNonUserCodeAttribute"
    ; StepThroughCtor = getSingleCtor "System.Diagnostics.DebuggerStepThroughAttribute"
    ; LispExport = getSingleCtor "Serehfa.Attributes.LispExportAttribute"
    ; LispLibrary = getSingleCtor "Serehfa.Attributes.LispLibraryAttribute"
    ; AssmConfigCtor = getSingleCtor "System.Reflection.AssemblyConfigurationAttribute"
    ; EnvTy = null
    ; Builtins = Map.empty }

// --------------------  Builtin Macro Definitions -----------------------------


/// Parse a builtin macro from syntax rules
let private parseBuiltinMacro id rules =
    let (node, errs) =
        Syntax.readExpr1 (sprintf "builtin-%s" id) rules
    if Diagnostics.hasErrors errs then
        failwithf "ICE: Error in builtin macro: %A" errs
    match node with
    | { Kind = AstNodeKind.Seq([n])} -> n
    | n -> n
    |> Macros.parseSyntaxRules id
    |> ResultEx.unwrap


/// Builtin `and` Macro
let private macroAnd =
    "(syntax-rules ::: ()
        ((_ a) a)
        ((_ a b :::) (if a (and b :::) #f))
        ((_) #t))"
    |> parseBuiltinMacro "and"

/// Builtin `or` Macro
let private macroOr =
    "(syntax-rules ()
        ((or) #f)
        ((or test) test)
        ((or test1 test2 ...)
            (let ((|90a3b246-0d7b-4f47-8e1e-0a9f0e7e3288| test1))
                (if |90a3b246-0d7b-4f47-8e1e-0a9f0e7e3288| |90a3b246-0d7b-4f47-8e1e-0a9f0e7e3288| (or test2 ...)))))"
    |> parseBuiltinMacro "or" 

/// Builtin `when` Macro
let private macroWhen =
    "(syntax-rules ()
        ((_ cond expr expr1 ...)
         (if cond
            (begin
                expr
                expr1 ...))))"
    |> parseBuiltinMacro "when"

/// Builtin `unless` Macro
let private macroUnless =
    "(syntax-rules ()
        ((_ cond expr expr1 ...)
         (if (not cond)
            (begin
                expr
                expr1 ...))))"
    |> parseBuiltinMacro "unless"

// FIXME: The following two hardcoded locations should be replaced by some kind
//        of SDK resoltuion.
let private serehfaAssmLoc = typeof<Serehfa.ConsPair>.Assembly.Location
let private mscorelibAssmLoc = typeof<obj>.Assembly.Location

/// The list of builtin macros
let private coreMacros =
    { LibraryName = ["scheme";"base"]
    ; Exports =
        [ macroAnd ; macroOr; macroWhen; macroUnless ]
        |> List.map (fun m -> (m.Name, StorageRef.Macro(m))) }
    |> Seq.singleton

// ------------------------ Public Builtins API --------------------------------

/// Load the signature from a given libary name
let public loadReferencedSignatures (name: string) =
    use assm =
        Mono.Cecil.AssemblyDefinition.ReadAssembly(name, assmReadParams)
    assm.MainModule.Types
    |> Seq.choose tryGetSignatureFromType
    |> List.ofSeq

/// The core library signature
let public loadCoreSignatures =
    let coreMethods =
        loadReferencedSignatures serehfaAssmLoc
        |> Seq.map (fun (_, lib) -> lib)
    Seq.append coreMethods coreMacros
    |> Seq.groupBy (fun lib -> lib.LibraryName)
    |> Seq.map (fun (name, parts) ->
        let bodies =
            parts
            |> Seq.collect (fun p -> p.Exports)
            |> List.ofSeq
        { LibraryName = name; Exports = bodies })
    |> List.ofSeq

/// Load the core types into the given assembly
let importCore (targetAssm: AssemblyDefinition) =
    use sehrefaAssm =
        AssemblyDefinition.ReadAssembly(serehfaAssmLoc, assmReadParams)
    use mscorelibAssm =
        AssemblyDefinition.ReadAssembly(mscorelibAssmLoc, assmReadParams)
    let builtins = loadExternBuiltins targetAssm sehrefaAssm
    { loadCoreTypes targetAssm [ sehrefaAssm ; mscorelibAssm ]
        with EnvTy = addEnvDecls targetAssm; Builtins = builtins }
