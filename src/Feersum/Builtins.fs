module Builtins

open IlHelpers
open System.Reflection
open Mono.Cecil
open Mono.Cecil.Rocks
open Mono.Cecil.Cil
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
    ; Builtins: Map<string,MethodReference> }


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

let private findBuiltinMethods (externAssm: Assembly) =
    let findBuiltinMethodsForTy (ty: Type) =
            ty.GetMethods(BindingFlags.Public ||| BindingFlags.Static)
            |> Seq.map (fun m -> (m.GetCustomAttribute<Serehfa.Attributes.LispBuiltinAttribute>(), m))
            |> Seq.where (fun (attr, _) -> not (isNull attr))
    externAssm.ExportedTypes
    |> Seq.collect findBuiltinMethodsForTy

let private loadExternBuiltins (lispAssm: AssemblyDefinition) (externAssm: Assembly) =
    findBuiltinMethods externAssm
    |> Seq.map (fun (a, m) -> (a.Name, lispAssm.MainModule.ImportReference(m)))
    |> Map.ofSeq

let private loadCoreTypes (lispAssm: AssemblyDefinition) (externAssm: Assembly) =
    let consTy = lispAssm.MainModule.ImportReference(externAssm.GetType("Serehfa.ConsPair")).Resolve()
    let consCtor =
        consTy.GetConstructors()
        |> Seq.head
        |> lispAssm.MainModule.ImportReference
        
    let identTy = lispAssm.MainModule.ImportReference(externAssm.GetType("Serehfa.Ident")).Resolve()
    let identCtor =
        identTy.GetConstructors()
        |> Seq.head
        |> lispAssm.MainModule.ImportReference

    let undefinedTy = lispAssm.MainModule.ImportReference(externAssm.GetType("Serehfa.Undefined")).Resolve()
    let undefinedInstance =
        undefinedTy.GetMethods()
        |> Seq.find (fun x -> x.Name = "get_Instance")
        |> lispAssm.MainModule.ImportReference

    { ConsTy = consTy
    ; ConsCtor = consCtor
    ; IdentCtor = identCtor
    ; UndefinedInstance = undefinedInstance
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

let private serehfaAssm = typeof<Serehfa.ConsPair>.Assembly

/// The list of builtin procedure names
let coreProcedures =
    findBuiltinMethods serehfaAssm
    |> Seq.map (fun (a, _) -> (a.Name, StorageRef.Builtin(a.Name)))

/// The list of builtin macros
let coreMacros =
    [ macroAnd ; macroOr; macroWhen; macroUnless ]
    |> Seq.map (fun m -> (m.Name, StorageRef.Macro(m)))

// ------------------------ Public Builtins API --------------------------------

/// The core library signature
let public loadCoreSignature =
    { LibraryName = ["scheme";"base"]
    ; Exports = Seq.append coreProcedures coreMacros |> List.ofSeq }

/// Load the core types into the given assembly
let importCore (assm: AssemblyDefinition) =
    let builtins = loadExternBuiltins assm serehfaAssm
    { loadCoreTypes assm serehfaAssm with EnvTy = addEnvDecls assm; Builtins = builtins }
