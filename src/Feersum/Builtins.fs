module Builtins

open IlHelpers
open System.Reflection
open Mono.Cecil
open Mono.Cecil.Cil
open System

/// Create all builtin methods
/// 
/// Returns a map containing all of the builtin methods supported by the
/// implementation.
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

    /// Arithmetic builtin. Combines all elements in the arguments array with
    /// the given `opcode`. If only 1 element is provided then `def` is used to
    /// compute some form of inverse. If no arguments are provided then `def` is
    /// returned.
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

    /// Display builtin. This is intended for user-readable output rather than
    /// any machine readable round tripping. Printing out strings & chars should
    /// display their raw form. All other objects is up to the implementation.
    /// 
    /// This implementation calls `ToString` on the underlying .NET object and
    /// uses that directly.
    let displayBuiltin =
        let meth, il = declareBuiltinMethod "display"
        let fail = il.Create(OpCodes.Nop)
        let hasValue = il.Create(OpCodes.Nop)
        let print = il.Create(OpCodes.Dup)

        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldlen)
        il.Emit(OpCodes.Ldc_I4_1)
        il.Emit(OpCodes.Bne_Un, fail)

        // null check
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Ldelem_Ref)
        il.Emit(OpCodes.Brtrue_S, hasValue)

        // If null use empty string
        il.Emit(OpCodes.Ldstr, "")
        il.Emit(OpCodes.Br, print)

        // convert to string
        il.Append(hasValue)
        let toStr = typeof<obj>.GetMethod("ToString", BindingFlags.Public ||| BindingFlags.Instance)
        let toStr = assm.MainModule.ImportReference toStr
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Ldelem_Ref)
        il.Emit(OpCodes.Callvirt, toStr)

        il.Append(print)

        let write = typeof<Console>.GetMethod("WriteLine", [| typeof<string> |])
        let write = assm.MainModule.ImportReference write
        il.Emit(OpCodes.Call, write)
        il.Emit(OpCodes.Ret)

        il.Append(fail)
        emitThrow il assm "`dsiplay` expects a single argument"

        meth

    [ ("+", createArithBuiltin "+" OpCodes.Add 0.0)
    ; ("-", createArithBuiltin "-" OpCodes.Sub 0.0)
    ; ("/", createArithBuiltin "/" OpCodes.Div 1.0)
    ; ("*", createArithBuiltin "*" OpCodes.Mul 1.0)
    ; ("=", createCompBuiltin "aritheq" OpCodes.Beq)
    ; (">", createCompBuiltin "arithgt" OpCodes.Bgt)
    ; ("<", createCompBuiltin "arithlt" OpCodes.Blt)
    ; (">=", createCompBuiltin "arithgte" OpCodes.Bge)
    ; ("<=", createCompBuiltin "arithlte" OpCodes.Ble)
    ; ("display", displayBuiltin) ]
    |> Map.ofSeq
