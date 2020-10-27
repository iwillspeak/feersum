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

/// Create all builtin methods
/// 
/// Returns a map containing all of the builtin methods supported by the
/// implementation.
let private createBuiltins (assm: AssemblyDefinition) (ty: TypeDefinition) =
    let declareBuiltinMethod name =
        let meth = MethodDefinition(name,
                                MethodAttributes.Public ||| MethodAttributes.Static,
                                assm.MainModule.TypeSystem.Object)
        let args = ParameterDefinition(ArrayType(assm.MainModule.TypeSystem.Object))
        meth.Parameters.Add(args)
        ty.Methods.Add meth
        let il = meth.Body.GetILProcessor()
        (meth, il)

    /// Comparator Builtin. Builds a method which uses the given `op` to compare
    /// all arguments and returns a boolean result. If 0 or 1 elements are
    /// given the method will always return `#t`.
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

    [ ("=", createCompBuiltin "aritheq" OpCodes.Beq)
    ; (">", createCompBuiltin "arithgt" OpCodes.Bgt)
    ; ("<", createCompBuiltin "arithlt" OpCodes.Blt)
    ; (">=", createCompBuiltin "arithgte" OpCodes.Bge)
    ; ("<=", createCompBuiltin "arithlte" OpCodes.Ble) ]
    |> Seq.map(fun (name, method) -> (name, method :> MethodReference))
    |> Map.ofSeq


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

let private loadExternBuiltins (lispAssm: AssemblyDefinition) (externAssm: Assembly) =
    let findBuiltinMethods (ty: Type) =
        ty.GetMethods(BindingFlags.Public ||| BindingFlags.Static)
        |> Seq.map (fun m -> (m.GetCustomAttribute<Serehfa.LispBuiltinAttribute>(), m))
        |> Seq.where (fun (attr, _) -> not (isNull attr))
        |> Seq.map (fun (attr, method) ->
            (attr.Name, lispAssm.MainModule.ImportReference(method)))

    externAssm.ExportedTypes
    |> Seq.collect findBuiltinMethods
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

let loadCore (assm: AssemblyDefinition) (ty: TypeDefinition) =
    let serehfaAssm = typeof<Serehfa.Class1>.Assembly
    let externalBuiltins = loadExternBuiltins assm serehfaAssm
    let internalBuiltins = createBuiltins assm ty
    let builtins =
        Map.toSeq internalBuiltins
        |> Seq.append (Map.toSeq externalBuiltins)
        |> Map.ofSeq
    { loadCoreTypes assm serehfaAssm with EnvTy = addEnvDecls assm; Builtins = builtins }