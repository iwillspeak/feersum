module Compile

open Bind
open Syntax
open System.IO
open System
open Mono.Cecil
open Mono.Cecil.Rocks
open Mono.Cecil.Cil

// Type to Hold Context While Emitting IL
type EmitCtx = { Assm: AssemblyDefinition; IL: ILProcessor }

/// Emit an instance of the unspecified value
let private emitUnspecified (il: ILProcessor) =
    // TODO: What should we do about empty sequences? This falls back to '()
    il.Emit(OpCodes.Ldnull)

/// Emit a Single Bound Expression
/// 
/// Emits the code for a single function into the given assembly.
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
    | BoundExpr.Seq [] -> emitUnspecified ctx.IL
    | BoundExpr.Seq s -> emitSequence ctx s
    | BoundExpr.Application(ap, args) -> emitApplication ctx ap args
    | BoundExpr.Definition(id, storage, maybeVal) ->
        match storage with
        | StorageRef.Global id -> failwith "globals not implemented"
        | StorageRef.Local(Local.Local idx) -> 
            match maybeVal with
            | Some(expr) -> recurse expr
            | None -> ctx.IL.Emit(OpCodes.Ldnull) // TODO: default values?
            ctx.IL.Emit(OpCodes.Stloc, idx)
    | BoundExpr.Load storage ->
        match storage with
        | StorageRef.Global id -> failwith "globals not implemented"
        | StorageRef.Local(Local.Local idx) -> ctx.IL.Emit(OpCodes.Ldloc, idx)
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
and emitSequence ctx seq =
    let popAndEmit x =
        ctx.IL.Emit(OpCodes.Pop)
        emitExpression ctx x
    emitExpression ctx (List.head seq)
    List.tail seq
    |>  Seq.iter popAndEmit
and emitApplication ctx ap args =
    match ap with
    | _ -> failwith "application not implemented"

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
    let ctor = MethodDefinition(".ctor", MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.SpecialName ||| MethodAttributes.RTSpecialName, assm.MainModule.TypeSystem.Void)
    let objConstructor = assm.MainModule.ImportReference((typeof<obj>).GetConstructor(Array.empty))
    let il = ctor.Body.GetILProcessor()
    il.Emit(OpCodes.Ldarg_0)
    il.Emit(OpCodes.Call, objConstructor)
    il.Emit(OpCodes.Ret)
    ctor

/// Emit a Bound Expression to .NET
/// 
/// Creates an assembly and writes out the .NET interpretation of the
/// given bound tree.
/// 
/// TODO: this method is a huge mess. The creation of the types, methods and other
/// supporting work nees extracting and abstracting to make thigs simpler. We will
/// need some of this to be shared when emiting methods and closures too.
let emit (outputStream: Stream) outputName bound =
    // Create an assembly with a nominal version to hold our code    
    let name = AssemblyNameDefinition(outputName, Version(0, 1, 0))
    let assm = AssemblyDefinition.CreateAssembly(name, "lisp_module", ModuleKind.Console)

    // Genreate a nominal type to contain the methods for this program.        
    let progTy = TypeDefinition(outputName, "LispProgram", TypeAttributes.Class ||| TypeAttributes.Public ||| TypeAttributes.AnsiClass, assm.MainModule.TypeSystem.Object)
    assm.MainModule.Types.Add progTy
    progTy.Methods.Add <| createEmptyCtor assm

    // Emit the body of the script to a separate method so that the `Eval`
    // module can call it directly
    let bodyMethod = MethodDefinition("$ScriptBody", MethodAttributes.Public ||| MethodAttributes.Static, assm.MainModule.TypeSystem.Object)
    progTy.Methods.Add bodyMethod
    let ctx = { Assm = assm; IL = bodyMethod.Body.GetILProcessor() }
    emitExpression ctx bound
    ctx.IL.Emit(OpCodes.Ret)
    bodyMethod.Body.Optimize()

    // The `Main` method is the entry point of the program. It calls
    // `$ScriptBody` and coerces the return value to an exit code.
    let mainMethod = MethodDefinition("Main", MethodAttributes.Public ||| MethodAttributes.Static, assm.MainModule.TypeSystem.Int32)
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