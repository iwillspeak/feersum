module IlHelpers

open System
open Mono.Cecil
open Mono.Cecil.Cil
open Mono.Cecil.Rocks

/// Emit a sequence of instructions to throw an exception
let emitThrow (il: ILProcessor) (exCtor: MethodReference) (err: string) =
    il.Emit(OpCodes.Ldstr, err)
    il.Emit(OpCodes.Newobj, exCtor)
    il.Emit(OpCodes.Throw)

/// Create an Object Constructor
/// 
/// Provides a way to build a constructor for a given object. The constructor
/// is stubbed out to call the parent constructor first and then the `builder`
/// is called to fill in the constructor body. If no body is required then
/// the `createEmptyCtor` method can be used.
let createCtor (assm: AssemblyDefinition) builder =
    let ctor = MethodDefinition(".ctor",
                                MethodAttributes.Public |||
                                MethodAttributes.HideBySig |||
                                MethodAttributes.SpecialName |||
                                MethodAttributes.RTSpecialName,
                                assm.MainModule.TypeSystem.Void)

    let objConstructor = 
        assm.MainModule.TypeSystem.Object.Resolve().GetConstructors()
        |> Seq.find (fun x -> x.Parameters.Count = 0)
        |> assm.MainModule.ImportReference
    let il = ctor.Body.GetILProcessor()
    il.Emit(OpCodes.Ldarg_0)
    il.Emit(OpCodes.Call, objConstructor)

    builder ctor il

    il.Emit(OpCodes.Ret)
    ctor


/// Create an Empty Object Constructor
///
/// Creates a constructor method deifnition that just calls the parent
/// constructor. This is a convenience method for when the full `createCtor` is
/// not required.
let createEmptyCtor (assm: AssemblyDefinition) =
    createCtor assm (fun _ _ -> ())

/// Create a `ParameterDefinition` with the given `name` and `ty`.
let namedParam name ty =
    ParameterDefinition(name,
                        ParameterAttributes.None,
                        ty)

/// Convert a method reference on a generic type to a method reference on a bound
/// generic instance type.
/// 
/// https://stackoverflow.com/a/16433452/1353098   - CC BY-SA 4.0
let makeHostInstanceGeneric args (method: MethodReference) =
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