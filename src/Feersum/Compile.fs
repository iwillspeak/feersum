module Compile

open Bind
open Syntax
open System.IO
open System
open Mono.Cecil
open Mono.Cecil.Cil

/// Emit a Single Bound Expression
/// 
/// Emits the code for a single function into the given assembly.
let rec emitExpression (assm: AssemblyDefinition) (il: ILProcessor) (expr: BoundExpr) =
    let recurse = emitExpression assm il
    match expr with
    | BoundExpr.Null -> il.Emit(OpCodes.Ldnull)
    | BoundExpr.Number n ->
        il.Emit(OpCodes.Ldc_R8, n)
        il.Emit(OpCodes.Box, assm.MainModule.TypeSystem.Double)
    | BoundExpr.Str s -> il.Emit(OpCodes.Ldstr, s)
    | BoundExpr.Boolean b ->
        il.Emit(if b then OpCodes.Ldc_I4_1 else OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Box, assm.MainModule.TypeSystem.Boolean)
    | BoundExpr.Seq s ->
        emitSequence assm il s
    | BoundExpr.Application(ap, args) -> emitApplication assm il ap args
    | BoundExpr.Definition(id, storage, maybeVal) ->
        match storage with
        | StorageRef.Global id -> failwith "globals not implemented"
        | StorageRef.Local idx -> 
            match maybeVal with
            | Some(expr) -> recurse expr
            | None -> il.Emit(OpCodes.Ldnull) // TODO: default values?
            il.Emit(OpCodes.Stloc, idx)
    | BoundExpr.Load storage ->
        match storage with
        | StorageRef.Global id -> failwith "globals not implemented"
        | StorageRef.Local idx -> il.Emit(OpCodes.Ldloc, idx)
    | BoundExpr.If(cond, ifTrue, maybeIfFalse) ->
        recurse cond
        let lblFalse = il.Create(OpCodes.Nop)
        let lblEnd = il.Create(OpCodes.Nop)
        il.Emit(OpCodes.Brfalse_S, lblFalse)
        recurse ifTrue
        il.Emit(OpCodes.Br_S, lblEnd)
        il.Append(lblFalse)
        match maybeIfFalse with
        | Some ifFalse -> emitExpression assm  il ifFalse
        | None -> il.Emit(OpCodes.Ldnull)
        il.Append(lblEnd)
and emitSequence assm il seq =
    let popAndEmit x =
        il.Emit(OpCodes.Pop)
        emitExpression assm il x
    emitExpression assm il (List.head seq)
    List.tail seq
    |>  Seq.iter popAndEmit
and emitApplication assm il ap args =
    match ap with
    | _ -> ()

/// Emit a Bound Expression to .NET
/// 
/// Creates an assembly and writes out the .NET interpretation of the
/// given bound tree.
/// 
/// TODO: this method is a huge mess. The creation of the types, methods and other
/// supporting work nees extracting and abstracting to make thigs simpler. We will
/// need some of this to be shared when emiting methods and closures too.
let emit path bound =
    let stem = Path.GetFileNameWithoutExtension((string)path);

    // Create an assembly with a nominal version to hold our code    
    let name = AssemblyNameDefinition(stem, Version(0, 1, 0))
    let assm = AssemblyDefinition.CreateAssembly(name, path, ModuleKind.Console)

    // Genreate a nominal type and main method        
    let progTy = TypeDefinition(stem, "LispProgram", TypeAttributes.Class ||| TypeAttributes.Public ||| TypeAttributes.AnsiClass, assm.MainModule.TypeSystem.Object)
    assm.MainModule.Types.Add progTy
    let ctor = MethodDefinition(".ctor", MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.SpecialName ||| MethodAttributes.RTSpecialName, assm.MainModule.TypeSystem.Void)
    let objConstructor = assm.MainModule.ImportReference((typeof<obj>).GetConstructor(Array.empty))
    let il = ctor.Body.GetILProcessor()
    il.Emit(OpCodes.Ldarg_0)
    il.Emit(OpCodes.Call, objConstructor)
    il.Emit(OpCodes.Ret)
    progTy.Methods.Add ctor

    let mainMethod = MethodDefinition("Main", MethodAttributes.Public ||| MethodAttributes.Static, assm.MainModule.TypeSystem.Int32)
    mainMethod.Parameters.Add(ParameterDefinition(ArrayType(assm.MainModule.TypeSystem.String)))
    let il = mainMethod.Body.GetILProcessor()
    progTy.Methods.Add mainMethod

    emitExpression assm il bound

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

    assm.EntryPoint <- mainMethod

    // TOOD: The way the file gets written, and the metadata to write with it, nees to be
    //       abstracted to deal with different target framework's prefrences. For now the
    //       `.exe` we generate is compatible with .NET Core and Mono. It would be nice
    //       to make this explicit somewhere in future.    
    assm.Write path
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
    """)

/// Compile a single AST node into an assembly
///
/// The plan for this is we make multiple passes over the syntax tree. First
/// pass will be to `bind` theh tree. Resulting in a `BoundExpr`. This will
/// attach any type information that _can_ be computed to each node, and
/// resolve variable references to the symbols that they refer to.
/// 
/// Once the expression is bound we will then `emit` the expression this walks
/// the expression and writes out the corresponding .NET IL to an `Assembly`
/// at `output`.
let compile node output =
    let scope = createRootScope
    bind scope node
    |> emit output

/// Read a File and Compile
/// 
/// Takes the `path` to an input to read and compile.
let compileFile(path: string) =
    let output = Path.GetFileNameWithoutExtension(path) + ".exe"
    parseFile path
    |> Result.map (fun ast -> compile ast output)
