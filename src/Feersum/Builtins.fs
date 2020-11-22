module Builtins

open IlHelpers
open System.Reflection
open Mono.Cecil
open Mono.Cecil.Rocks
open Mono.Cecil.Cil
open System

/// Core Types Used by the Compiler at Runtime
type CoreTypes =
    { ConsTy: TypeDefinition
    ; EnvTy: TypeDefinition
    ; ConsCtor: MethodReference
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
            |> Seq.map (fun m -> (m.GetCustomAttribute<Serehfa.LispBuiltinAttribute>(), m))
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

    { ConsTy = consTy
    ; ConsCtor = consCtor
    ; IdentCtor = identCtor
    ; EnvTy = null
    ; Builtins = Map.empty }

let private serehfaAssm = typeof<Serehfa.ConsPair>.Assembly

// ------------------------ Public Builtins API --------------------------------

/// The list of builtin procedure names
let public coreProcNames =
    findBuiltinMethods serehfaAssm
    |> Seq.map (fun (a, _) -> a.Name)


/// Load the core types into the given assembly
let loadCore (assm: AssemblyDefinition) =
    let builtins = loadExternBuiltins assm serehfaAssm
    { loadCoreTypes assm serehfaAssm with EnvTy = addEnvDecls assm; Builtins = builtins }
