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
    { ConsTy: TypeDefinition
    ; EnvTy: TypeDefinition
    ; ConsCtor: MethodReference
    ; UndefinedInstance: MethodReference
    ; IdentCtor: MethodReference
    ; FuncObjCtor: MethodReference
    ; FuncObjInvoke: MethodReference
    ; ExceptionCtor: MethodReference
    ; Builtins: Map<string,MethodReference> }

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
        ty.Fields |> chooseMatching "LispExportAttribute" (onGlobal ty)
    let builtins =
        ty.Methods |> chooseMatching "LispBuiltinAttribute" (onBuiltin)

    Seq.append exports builtins

/// Get Exported items from a given Mono type definition.
let private getExports =
    cataExports (fun ty name field -> (name, Global(ty.FullName, field.Name))) (fun name _ -> (name, Builtin(name)))
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
    let onTy ty name =
        ty
        |> cataExports (fun _ _ _ -> None) (fun id method -> Some((id, method)))
        |> Seq.choose id
    externAssm.MainModule.Types
    |> Seq.choose (tryCataType onTy)
    |> Seq.concat
    |> Seq.map (fun (name, m) -> (name, lispAssm.MainModule.ImportReference(m :> MethodReference)))
    |> Map.ofSeq

let private loadCoreTypes (lispAssm: AssemblyDefinition) (externAssms: seq<AssemblyDefinition>) =
    let getType name =
        externAssms
        |> Seq.pick (fun (assm: AssemblyDefinition) ->
            assm.MainModule.Types
            |> Seq.tryFind (fun x  -> x.FullName = name))
    let consTy = lispAssm.MainModule.ImportReference(getType "Serehfa.ConsPair").Resolve()
    let consCtor =
        consTy.GetConstructors()
        |> Seq.head
        |> lispAssm.MainModule.ImportReference
        
    let identTy = lispAssm.MainModule.ImportReference(getType "Serehfa.Ident").Resolve()
    let identCtor =
        identTy.GetConstructors()
        |> Seq.head
        |> lispAssm.MainModule.ImportReference

    let exTy = lispAssm.MainModule.ImportReference(getType "System.Exception").Resolve()
    let exCtor =
        exTy.GetConstructors()
        |> Seq.find (fun x -> x.Parameters.Count = 1 && x.Parameters.[0].ParameterType.Name = "String")
        |> lispAssm.MainModule.ImportReference

    // FIXME: work out generics
    let paramTypes = [|typeof<obj>; typeof<IntPtr>|]
    let funcObjCtor = typeof<Func<obj[], obj>>.GetConstructor(paramTypes)
    let funcObjCtor = lispAssm.MainModule.ImportReference(funcObjCtor)
    let funcInvoke = typeof<Func<obj[], obj>>.GetMethod("Invoke",
                                                        [| typeof<obj[]> |])
    let funcInvoke = lispAssm.MainModule.ImportReference(funcInvoke)
    // ENDFIXME

    let undefinedTy = lispAssm.MainModule.ImportReference(getType "Serehfa.Undefined").Resolve()
    let undefinedInstance =
        undefinedTy.GetMethods()
        |> Seq.find (fun x -> x.Name = "get_Instance")
        |> lispAssm.MainModule.ImportReference

    { ConsTy = consTy
    ; ConsCtor = consCtor
    ; IdentCtor = identCtor
    ; UndefinedInstance = undefinedInstance
    ; FuncObjCtor = funcObjCtor
    ; FuncObjInvoke = funcInvoke
    ; ExceptionCtor = exCtor
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
