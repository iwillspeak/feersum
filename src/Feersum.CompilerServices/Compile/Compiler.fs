namespace Feersum.CompilerServices.Compile

open System
open System.IO
open Mono.Cecil
open Mono.Cecil.Rocks
open Mono.Cecil.Cil
open System.Collections.Generic

open Feersum.CompilerServices
open Feersum.CompilerServices.Ice
open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Text
open Feersum.CompilerServices.Binding
open Feersum.CompilerServices.Syntax
open Feersum.CompilerServices.Compile.MonoHelpers
open Feersum.CompilerServices.Targets
open Feersum.CompilerServices.Utils
open Feersum.CompilerServices.Syntax.Tree

/// Capture Environment Kind
///
/// Capture environments are either standard environments containing fields used
/// to store captured values; or link environments.
type EnvInfo =
    /// A standard environment represents a closure type containing each
    /// captured value as a field. If the environment has a parent then that is
    /// stored as the final field.
    | Standard of local: VariableDefinition * ty: TypeDefinition * parent: EnvInfo option
    /// Link environments represent the case when only the parent environment
    /// is captured. In this case we can re-use the parent type and pointer at
    /// runtime. This saves the cost of a new type just for this clousure's
    /// methods, and saves indirection on lookups for deeply nested captures.
    | Link of EnvInfo

module private EnvUtils =
    /// Get the local variable used to store the current method's environment
    let getLocal =
        function
        | Standard(local, _, _) -> local |> Some
        | _ -> None

    /// Get the type part of the environment info.
    let rec getType =
        function
        | Standard(_, ty, _) -> ty
        | Link inner -> getType inner

    /// Get the parent of the given environment.
    let rec getParent =
        function
        | Standard(_, _, parent) -> parent
        | Link inner -> Some(inner)

/// Type to Hold Context While Emitting IL
type EmitCtx =
    { Assm: AssemblyDefinition
      IL: ILProcessor
      Locals: VariableDefinition list
      Parameters: ParameterDefinition list
      DebugDocuments: Dictionary<string, Document>
      mutable NextLambda: int
      ScopePrefix: string
      EmitSymbols: bool
      Exports: Map<string, string>
      Externs: Map<string, TypeDefinition>
      ParentEnvironment: EnvInfo option
      Environment: EnvInfo option
      ProgramTy: TypeDefinition
      mutable Initialisers: Map<string, MethodReference>
      mutable Libraries: Map<string, TypeDefinition>
      Core: CoreTypes }

type CompileResult =
    { Diagnostics: Diagnostic list
      EmittedAssemblyName: AssemblyNameDefinition option }

[<RequireQualifiedAccess>]
type CompileInput =
    | Program of (TextDocument * Program) list
    | Script of TextDocument * ScriptProgram

[<AutoOpen>]
module private Utils =

    /// Get the Parent Environment from the current emit context
    let getParentEnv ctx = ctx.ParentEnvironment

    /// Predicate to check if the current emit context has a parent environment.
    let hasParentEnv = getParentEnv >> Option.isSome

    /// Predicate to check if the current emit context has a capture environment.
    let hasEnv ctx = ctx.Environment |> Option.isSome

    /// Set the attributes on a given method to mark it as compiler generated code
    let markAsCompilerGenerated (core: CoreTypes) (method: MethodDefinition) =
        let addSimpleAttr (attrCtor: MethodReference) =
            attrCtor |> CustomAttribute |> method.CustomAttributes.Add

        addSimpleAttr core.CompGenCtor
        addSimpleAttr core.NonUserCodeCtor
        addSimpleAttr core.StepThroughCtor

    /// Mark a type as compiler generated.
    let markTypeAsCompilerGenerated (core: CoreTypes) (ty: TypeDefinition) =
        core.CompGenCtor |> CustomAttribute |> ty.CustomAttributes.Add

    /// Mark the assembly as supporting debugging
    let markAsDebuggable (core: CoreTypes) (assm: AssemblyDefinition) =
        let attr = core.DebuggableCtor |> CustomAttribute
        attr.ConstructorArguments.Add(CustomAttributeArgument(assm.MainModule.TypeSystem.Boolean, true))
        attr.ConstructorArguments.Add(CustomAttributeArgument(assm.MainModule.TypeSystem.Boolean, true))

        attr |> assm.CustomAttributes.Add

        let attr = core.AssmConfigCtor |> CustomAttribute
        attr.ConstructorArguments.Add(CustomAttributeArgument(assm.MainModule.TypeSystem.String, "Debug"))

        attr |> assm.CustomAttributes.Add

    /// Mark the assembly with `[CompilationRelaxations]` to indicate we don't
    /// require string interning.
    let markCompilationRelaxations (core: CoreTypes) (assm: AssemblyDefinition) =
        let attr = core.CompRelaxAttr |> CustomAttribute

        attr.ConstructorArguments.Add(
            CustomAttributeArgument(
                core.CompRelaxations,
                Runtime.CompilerServices.CompilationRelaxations.NoStringInterning
            )
        )

        attr |> assm.CustomAttributes.Add

    /// Mark the given type with a library `name`. This adds the
    /// `LispLibrary` attribute to the type.
    let private markWithLibraryName (core: CoreTypes) (typ: TypeDefinition) name =
        let attr = core.LispLibrary |> CustomAttribute

        attr.ConstructorArguments.Add(
            CustomAttributeArgument(
                typ.Module.TypeSystem.String.MakeArrayType(),
                name
                |> Seq.map (fun namePart -> CustomAttributeArgument(typ.Module.TypeSystem.String, namePart))
                |> Array.ofSeq
            )
        )

        attr |> typ.CustomAttributes.Add

    /// Add an attribute marking the given item as re-exported under the new
    /// name `ext` from `typ`.
    let private markAsReExport (core: CoreTypes) (typ: TypeDefinition) ext libTy item =
        let attr = core.LispReExport |> CustomAttribute
        attr.ConstructorArguments.Add(CustomAttributeArgument(typ.Module.TypeSystem.String, ext))
        attr.ConstructorArguments.Add(CustomAttributeArgument(core.TypeTy, libTy))

        match item with
        | Field id ->
            attr.ConstructorArguments.Add(CustomAttributeArgument(typ.Module.TypeSystem.String, id))
            attr.ConstructorArguments.Add(CustomAttributeArgument(typ.Module.TypeSystem.Boolean, false))
        | Method id ->
            attr.ConstructorArguments.Add(CustomAttributeArgument(typ.Module.TypeSystem.String, id))
            attr.ConstructorArguments.Add(CustomAttributeArgument(typ.Module.TypeSystem.Boolean, true))

        typ.CustomAttributes.Add(attr)

    /// Mark the field as exported. This adds the `LispExport` attribute
    /// to the field.
    let private markWithExportedName (core: CoreTypes) (field: FieldDefinition) name =
        let attr = core.LispExport |> CustomAttribute
        attr.ConstructorArguments.Add(CustomAttributeArgument(field.Module.TypeSystem.String, name))
        attr |> field.CustomAttributes.Add

    /// Emit an instance of the unspecified value
    let private emitUnspecified ctx =
        ctx.IL.Emit(OpCodes.Call, ctx.Core.UndefinedInstance)

    /// Find an extern type by full `typeName`.
    let private getExternType ctx typeName =
        match Map.tryFind typeName ctx.Externs with
        | Some prefix -> prefix
        | None -> icef "Attempt to access external type '%s', which is not yet imported" typeName

    /// Find an extern method by `id`
    let private getExternMethod ctx typeName id =
        let ty = getExternType ctx typeName

        let method = ty.GetMethods() |> Seq.find (fun m -> m.Name = id)

        ctx.Assm.MainModule.ImportReference(method)

    /// Find an extern field by `id`
    let private getExternField ctx typeName id =
        let ty = getExternType ctx typeName

        let field = ty.Fields |> Seq.find (fun f -> f.Name = id)

        ctx.Assm.MainModule.ImportReference(field)

    /// Ensure a field exists on the program type to be used as a global variable
    let private getField ctx typeName id =
        match Map.tryFind typeName ctx.Libraries with
        | Some ty ->
            ty.Fields
            |> Seq.tryFind (fun f -> f.Name = id)
            |> Option.defaultWith (fun () ->
                let newField =
                    FieldDefinition(id, FieldAttributes.Static, ctx.Assm.MainModule.TypeSystem.Object)

                if ctx.ProgramTy <> ty then
                    icef "Type %A does not match %A being modified for field %s" ctx.ProgramTy ty id

                ty.Fields.Add(newField)
                let export = Map.tryFind id ctx.Exports

                match export with
                | Some(exportedName) ->
                    markWithExportedName ctx.Core newField exportedName
                    newField.Attributes <- newField.Attributes ||| FieldAttributes.Public
                    ()
                | None -> ()

                newField)
            :> FieldReference
        | None -> getExternField ctx typeName id

    /// Convert an argument index into a `ParameterDefinition` for `Ldarg` given the
    /// current context. This indexes into the context's `Parameters` list.
    let private argToParam ctx idx = ctx.Parameters[idx]

    /// Convert a local index into a `VariableDefinition`. Variable definitions can
    /// then be used with `Ldloc` and `Stloc`.
    let private localToVariable ctx idx = ctx.Locals[idx]

    /// Emit a sequence of instructions to convert a method reference
    /// into a `Func<obj[],obj>` with the context as the current top of stack.
    let private emitMethodToInstanceFunc ctx (method: MethodReference) =
        ctx.IL.Emit(OpCodes.Ldftn, method)
        ctx.IL.Emit(OpCodes.Newobj, ctx.Core.FuncObjCtor)

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
    let private initialiseEnvironment
        ctx
        (envLocal: VariableDefinition)
        (ty: TypeDefinition)
        (parent: Option<EnvInfo>)
        (captured: Option<StorageRef list>)
        =

        if parent.IsSome then
            ctx.IL.Emit(OpCodes.Ldarg_0)

        let ctor = Seq.head <| ty.GetConstructors()

        ctx.IL.Emit(OpCodes.Newobj, ctor)
        ctx.IL.Emit(OpCodes.Stloc, envLocal)

        // Hoist any arguments into the environment
        captured
        |> Option.iter (
            Seq.iter (function
                | Environment(idx, Arg a) ->
                    ctx.IL.Emit(OpCodes.Ldloc, envLocal)
                    ctx.IL.Emit(OpCodes.Ldarg, argToParam ctx a)
                    ctx.IL.Emit(OpCodes.Stfld, ty.Fields[idx])
                | _ -> ())
        )

    /// Get the variable definition that the local environment is stored in.
    let private getEnvironment ctx =
        match ctx.Environment |> Option.bind (EnvUtils.getLocal) with
        | Some env -> env
        | None -> ice "Attempt to access environment in context without one"

    /// Walk the chain of captures starting at `from` in the parent, and then call
    /// `f` with the environment index that the capture chain ends at. This is used
    /// to read and write values from a captured environment.
    let rec private walkCaptureChain ctx envInfo from f =
        match from with
        | StorageRef.Captured(from) ->
            let parent =
                match envInfo with
                | Standard(_, ty, parent) ->
                    ctx.IL.Emit(OpCodes.Ldfld, ty.Fields[ty.Fields.Count - 1])
                    parent |> Option.unwrap
                | Link l -> l

            walkCaptureChain ctx parent from f
        | StorageRef.Environment(idx, _) -> f envInfo idx
        | _ -> icef "Unexpected storage in capture chain %A" from

    /// Given an environment at the top of the stack emit a load of the slot `idx`
    let private readFromEnv ctx envInfo (idx: int) =
        match envInfo with
        | Standard(_, ty, _) -> ctx.IL.Emit(OpCodes.Ldfld, ty.Fields[idx])
        | Link l -> icef "Attempt to read from link environment %A at index %d" l idx

    /// Given an environment at the top of the stack emit a store to the slot `idx`
    let private writeToEnv ctx (temp: VariableDefinition) envInfo (idx: int) =
        match envInfo with
        | Standard(_, ty, _) ->
            ctx.IL.Emit(OpCodes.Ldloc, temp)
            ctx.IL.Emit(OpCodes.Stfld, ty.Fields[idx])
        | Link l -> icef "Attempt to write to link environment %A at index %d" l idx

    let libraryTypeAttributes =
        TypeAttributes.Class
        ||| TypeAttributes.Public
        ||| TypeAttributes.AnsiClass
        ||| TypeAttributes.Abstract
        ||| TypeAttributes.Sealed

    /// Emit a Single Bound Expression
    ///
    /// Emits the code for a single function into the given assembly. For some more
    /// complex expression types it delegates to the mutually-recursive `emit*`s
    let rec private emitExpression (ctx: EmitCtx) tail (expr: BoundExpr) =
        let recurse = emitExpression ctx tail

        match expr with
        | BoundExpr.Nop -> emitUnspecified ctx
        | BoundExpr.Error -> ice "Attempt to lower an error expression"
        | BoundExpr.SequencePoint(inner, location) ->
            let pos = ctx.IL.Body.Instructions.Count
            recurse inner

            if ctx.EmitSymbols then
                let ins = ctx.IL.Body.Instructions[pos]

                if location = TextLocation.Missing then
                    let point = Cil.SequencePoint(ins, null)

                    point.StartLine <- 0xfeefee
                    point.EndLine <- 0xfeefee
                    point.StartColumn <- 0
                    point.EndColumn <- 0
                    ctx.IL.Body.Method.DebugInformation.SequencePoints.Add point

                else
                    let s = location.Start
                    let e = location.End

                    let mutable (found, doc) = ctx.DebugDocuments.TryGetValue(s.Source)

                    if not found then
                        doc <- Document(Path.GetFullPath(s.Source))
                        doc.Language <- DocumentLanguage.Other
                        doc.LanguageGuid <- Guid("c70c3e24-e471-4637-8129-10f771417dbb")
                        doc.LanguageVendor <- DocumentLanguageVendor.Other
                        doc.LanguageVendorGuid <- Guid("98378869-1abf-441b-9307-3bcca9a024cd")
                        // TODO: Set HashAlgorithm here, and store the SHA1 of the
                        //       debug document.
                        ctx.DebugDocuments[s.Source] <- doc

                    let point = Cil.SequencePoint(ins, doc)
                    point.StartLine <- int s.Line
                    point.StartColumn <- int s.Col
                    point.EndLine <- int e.Line
                    point.EndColumn <- int e.Col
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
            let lblEnd = ctx.IL.Create(OpCodes.Nop)
            let condTemp = makeTemp ctx ctx.Assm.MainModule.TypeSystem.Object

            // ILAAA       : <expression for cond>
            // ILAAA       : stloc condTemp
            // ILBBB       : ldloc condTemp
            // ILBBB       : isinst bool
            // ILBBB       : brfalse ILlblTrue
            // ILCCC       : ldloc condTemp
            // ILCCC       : brtrue ILlblTrue
            // ILDDD       : <false expression>
            // ILXXX       : br ILlblEnd ; or just `ret` for tail calls
            // ILlblTrue   : <true expression>
            // ILlblEnd    : nop ; only if not tail call

            emitExpression ctx false cond

            ctx.IL.Emit(OpCodes.Stloc, condTemp)
            ctx.IL.Emit(OpCodes.Ldloc, condTemp)
            ctx.IL.Emit(OpCodes.Isinst, ctx.Assm.MainModule.TypeSystem.Boolean)
            ctx.IL.Emit(OpCodes.Brfalse, lblTrue)

            ctx.IL.Emit(OpCodes.Ldloc, condTemp)
            ctx.IL.Emit(OpCodes.Unbox_Any, ctx.Assm.MainModule.TypeSystem.Boolean)
            ctx.IL.Emit(OpCodes.Brtrue, lblTrue)

            match maybeIfFalse with
            | Some ifFalse -> recurse ifFalse
            | None -> emitUnspecified ctx

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

            ctx.IL.Append(lblTrue)
            recurse ifTrue

            // We only need the branch target here if we emitted a jump to it above.
            if not tail then
                // TODO: Could do with another sequence point here at the join
                //       block. Or stop using `nops` all over the place as
                //       labels and instead.
                ctx.IL.Append(lblEnd)
        | BoundExpr.Lambda(formals, body) -> emitLambda ctx formals body
        | BoundExpr.Library(name, mangledName, exports, body) -> emitLibrary ctx name mangledName exports body
        | BoundExpr.Import name ->
            match Map.tryFind name ctx.Initialisers with
            | Some(initialiser) -> ctx.IL.Emit(OpCodes.Call, initialiser)
            | None -> emitUnspecified ctx
        | BoundExpr.Quoted quoted -> emitQuoted ctx quoted

    and emitQuoted ctx (quoted: BoundDatum) =
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

        match quoted with
        | BoundDatum.SelfEval c -> emitLiteral ctx c
        | BoundDatum.Compound [] -> emitLiteral ctx (BoundLiteral.Null)
        | BoundDatum.Compound s -> quoteSequence s
        | BoundDatum.Quoted q -> [ BoundDatum.Ident "quote"; q ] |> quoteSequence
        | BoundDatum.Ident id -> quoteIdent id

    and emitLiteral ctx =
        function
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
            |> List.fold
                (fun (idx: int) e ->
                    ctx.IL.Emit(OpCodes.Dup)
                    ctx.IL.Emit(OpCodes.Ldc_I4, idx)
                    emitQuoted ctx e
                    ctx.IL.Emit(OpCodes.Stelem_Ref)
                    idx + 1)
                0
            |> ignore
        | BoundLiteral.ByteVector bv ->
            let len = List.length bv

            // Create an empty array
            ctx.IL.Emit(OpCodes.Ldc_I4, len)
            ctx.IL.Emit(OpCodes.Newarr, ctx.Assm.MainModule.TypeSystem.Byte)

            if len > 0 then
                // Create a static copy of the bytes
                let literalTy =
                    TypeDefinition(
                        "Feersum.Internals",
                        sprintf "<>ByteLiteral%d" ctx.ProgramTy.Fields.Count,
                        TypeAttributes.NestedPrivate
                        ||| TypeAttributes.Sealed
                        ||| TypeAttributes.ExplicitLayout,
                        ctx.Core.ValueType
                    )

                literalTy.PackingSize <- 1s
                literalTy.ClassSize <- len
                ctx.ProgramTy.NestedTypes.Add(literalTy)

                let literalField =
                    FieldDefinition(
                        sprintf "<>/Lit%d" ctx.ProgramTy.Fields.Count,
                        FieldAttributes.Static
                        ||| FieldAttributes.InitOnly
                        ||| FieldAttributes.Assembly
                        ||| FieldAttributes.HasFieldRVA,
                        literalTy
                    )

                literalField.InitialValue <- List.toArray bv
                ctx.ProgramTy.Fields.Add(literalField)

                // Copy to our new array using the `InitializeArray` intrinsic.
                ctx.IL.Emit(OpCodes.Dup)
                ctx.IL.Emit(OpCodes.Ldtoken, literalField)
                ctx.IL.Emit(OpCodes.Call, ctx.Core.RuntimeInitArray)

    /// Emit a write to a given storage location
    and writeTo ctx storage =
        match storage with
        | StorageRef.Macro m -> icef "Can't re-define macro %s" m.Name
        | StorageRef.Global(mangledPrefix, loc) ->
            match loc with
            | Method id -> icef "Can't re-define builtin %s" id
            | Field id ->
                let field = getField ctx mangledPrefix id
                ctx.IL.Emit(OpCodes.Stsfld, field)
        | StorageRef.Local(idx) -> ctx.IL.Emit(OpCodes.Stloc, idx |> localToVariable ctx)
        | StorageRef.Arg(idx) -> ctx.IL.Emit(OpCodes.Starg, idx |> argToParam ctx)
        | StorageRef.Environment(idx, _) ->
            let temp = makeTemp ctx ctx.Assm.MainModule.TypeSystem.Object

            ctx.IL.Emit(OpCodes.Stloc, temp)

            ctx.IL.Emit(OpCodes.Ldloc, getEnvironment ctx)
            writeToEnv ctx temp (ctx.Environment |> Option.unwrap) idx
        | StorageRef.Captured(from) ->
            let temp = makeTemp ctx ctx.Assm.MainModule.TypeSystem.Object

            ctx.IL.Emit(OpCodes.Stloc, temp)

            // Start at the parent environment and walk up
            ctx.IL.Emit(OpCodes.Ldarg_0)
            walkCaptureChain ctx (getParentEnv ctx |> Option.unwrap) from (writeToEnv ctx temp)

    /// Emit a load from the given storage location
    and readFrom ctx storage =
        match storage with
        | StorageRef.Macro m -> icef "Invalid macro application %s" m.Name
        | StorageRef.Global(ty, loc) ->
            match loc with
            | Method id ->
                let meth = getExternMethod ctx ty id
                emitMethodToFunc ctx meth
            | Field id ->
                let field = getField ctx ty id
                ctx.IL.Emit(OpCodes.Ldsfld, field)
        | StorageRef.Local(idx) -> ctx.IL.Emit(OpCodes.Ldloc, idx |> localToVariable ctx)
        | StorageRef.Arg(idx) -> ctx.IL.Emit(OpCodes.Ldarg, idx |> argToParam ctx)
        | StorageRef.Environment(idx, _) ->
            ctx.IL.Emit(OpCodes.Ldloc, getEnvironment ctx)
            readFromEnv ctx (ctx.Environment |> Option.unwrap) idx
        | StorageRef.Captured(from) ->
            // start at the parent environment, and walk up
            ctx.IL.Emit(OpCodes.Ldarg_0)
            walkCaptureChain ctx (getParentEnv ctx |> Option.unwrap) from (readFromEnv ctx)

    /// Emit a Sequence of Expressions
    ///
    /// Emits each expression in the sequence, and pops any intermediate values
    /// from the stack. Emits the unspecified value if the sequence is empty.
    and emitSequence ctx tail seq =
        match seq with
        | [ one ] -> emitExpression ctx tail one
        | head :: rest ->
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

        // Duplicate the callee and check it is a faunction
        ctx.IL.Emit(OpCodes.Dup)
        ctx.IL.Emit(OpCodes.Isinst, ctx.Core.FuncObjTy)
        let doCall = ctx.IL.Create(OpCodes.Nop)
        ctx.IL.Emit(OpCodes.Brtrue_S, doCall)
        emitThrow ctx.IL ctx.Core.ExceptionCtor "Target is not a function and can't be applied"
        ctx.IL.Append(doCall)

        // Emit the arguments array
        ctx.IL.Emit(OpCodes.Ldc_I4, List.length args)
        ctx.IL.Emit(OpCodes.Newarr, ctx.Assm.MainModule.TypeSystem.Object)

        List.fold
            (fun (idx: int) e ->
                ctx.IL.Emit(OpCodes.Dup)
                ctx.IL.Emit(OpCodes.Ldc_I4, idx)
                emitExpression ctx false e
                ctx.IL.Emit(OpCodes.Stelem_Ref)
                idx + 1)
            0
            args
        |> ignore

        if tail then
            ctx.IL.Emit(OpCodes.Tail)

        ctx.IL.Emit(OpCodes.Callvirt, ctx.Core.FuncObjInvoke)

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

        let _, thunk =
            emitNamedLambda ctx (sprintf "%s:lambda%d" ctx.ScopePrefix lambdaId) formals body

        let method = thunk :> MethodReference

        // Create a `Func` instance with the appropriate `this` pointer.
        match ctx.Environment with
        | Some env ->
            // load the this pointer
            match env with
            | Standard(local, _, _) -> ctx.IL.Emit(OpCodes.Ldloc, local)
            | Link _ -> ctx.IL.Emit(OpCodes.Ldarg_0)

            emitMethodToInstanceFunc ctx method
        | _ -> emitMethodToFunc ctx method

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
    and public emitNamedLambda (ctx: EmitCtx) name formals root =

        let attrs =
            if hasEnv ctx then
                MethodAttributes.Public
            else
                MethodAttributes.Public ||| MethodAttributes.Static

        let methodDecl =
            MethodDefinition(name, attrs, ctx.Assm.MainModule.TypeSystem.Object)

        let mutable parameters = []

        let addParam id =
            let param = namedParam id ctx.Assm.MainModule.TypeSystem.Object

            methodDecl.Parameters.Add(param)
            parameters <- param :: parameters

        // Add formals as parameter definitions
        match formals with
        | Simple id -> addParam id
        | List fmls -> Seq.iter addParam fmls
        | DottedList(fmls, dotted) ->
            Seq.iter addParam fmls
            addParam dotted

        let mutable locals = []

        for _ = 1 to root.Locals do
            let local = VariableDefinition(ctx.Assm.MainModule.TypeSystem.Object)

            methodDecl.Body.Variables.Add(local)
            locals <- local :: locals

        /// Build an environment info for the given storage
        let buildEnv envSize =
            let parentTy = ctx.Environment |> Option.map (EnvUtils.getType)

            let envTy = sprintf "<%s>$Env" name |> makeEnvironmentType ctx.Assm parentTy envSize

            markTypeAsCompilerGenerated ctx.Core envTy

            let containerTy =
                ctx.Environment
                |> Option.map (EnvUtils.getType)
                |> Option.defaultValue ctx.ProgramTy

            containerTy.NestedTypes.Add envTy

            let envLocal = new VariableDefinition(envTy)
            methodDecl.Body.Variables.Add(envLocal)
            Standard(envLocal, envTy, ctx.Environment)

        let env =
            // Get get the size of environment object required to hold any captured values
            // in `envMappings`. The return from this has two main meanings:
            //
            //  * `0` -> Only the parent environment link is captured.
            //  * `n` -> An environment with `n` slots is required.
            let envSize =
                root.EnvMappings
                |> Option.map (
                    Seq.sumBy (function
                        | Environment _ -> 1
                        | _ -> 0)
                )

            match envSize with
            | Some(0) -> ctx.Environment |> Option.map Link
            | Some caps -> buildEnv caps |> Some
            | None -> None

        // Create a new emit context for the new method, and lower the body in that
        // new context.
        let ctx =
            { ctx with
                NextLambda = 0
                Locals = locals
                Parameters = List.rev parameters
                Environment = env
                ParentEnvironment = ctx.Environment
                IL = methodDecl.Body.GetILProcessor()
                ScopePrefix = name }

        match ctx.Environment with
        | Some e ->
            match e with
            | Standard(local, ty, parent) -> initialiseEnvironment ctx local ty parent root.EnvMappings
            | Link _ -> ()
        | None -> ()

        emitExpression ctx true root.Body
        ctx.IL.Emit(OpCodes.Ret)
        methodDecl.Body.Optimize()

        if ctx.EmitSymbols then
            let scope =
                ScopeDebugInformation(
                    methodDecl.Body.Instructions[0],
                    methodDecl.Body.Instructions[methodDecl.Body.Instructions.Count - 1]
                )

            // If we have an environment tell the debugger about it
            ctx.Environment
            |> Option.bind (EnvUtils.getLocal)
            |> Option.iter (fun env -> VariableDebugInformation(env, "capture-environment") |> scope.Variables.Add)

            ctx.Locals
            |> List.iteri (fun idx var -> VariableDebugInformation(var, sprintf "local%d" idx) |> scope.Variables.Add)

            methodDecl.DebugInformation.Scope <- scope

        // Emit a 'thunk' that unpacks the arguments to our method
        // This allows us to provide a uniform calling convention for
        // lambda instances.
        let thunkDecl =
            MethodDefinition((sprintf "%s:thunk" name), attrs, ctx.Assm.MainModule.TypeSystem.Object)

        thunkDecl.Parameters.Add(namedParam "args" (ArrayType(ctx.Assm.MainModule.TypeSystem.Object)))

        let thunkIl = thunkDecl.Body.GetILProcessor()

        /// Unpack a single argument from the arguments array onto the stack
        let unpackArg (idx: int) id =
            thunkIl.Emit(OpCodes.Ldarg, thunkDecl.Parameters[0])
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
            thunkIl.Emit(OpCodes.Ldarg, thunkDecl.Parameters[0])
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
            let loop = thunkIl.Create(OpCodes.Ldarg, thunkDecl.Parameters[0])

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

            thunkIl.Emit(OpCodes.Ldarg, thunkDecl.Parameters[0])
            thunkIl.Emit(OpCodes.Ldlen)
            thunkIl.Emit(OpCodes.Ldc_I4, count)
            thunkIl.Emit(opCode, ok)

            emitThrow thunkIl ctx.Core.ExceptionCtor err

            thunkIl.Append(ok)

        // If we are calling an instance method we need to push `this` on the stack
        // as the first argument before we unpack any others.
        if hasParentEnv ctx then
            thunkIl.Emit(OpCodes.Ldarg_0)

        match formals with
        | Simple id -> unpackRemainder 0
        | List fmls ->
            let expectedArgCount = List.length fmls

            raiseArgCountMismatch
                expectedArgCount
                OpCodes.Beq
                (sprintf "Expected exactly %d arguments" expectedArgCount)

            List.fold unpackArg 0 fmls |> ignore
        | DottedList(fmls, dotted) ->
            let expectedArgCount = List.length fmls

            raiseArgCountMismatch
                expectedArgCount
                OpCodes.Bge
                (sprintf "Expected at least %d arguments" expectedArgCount)

            let lastIdx = List.fold unpackArg 0 fmls
            unpackRemainder lastIdx

        // Call the real method as a tail call
        thunkIl.Emit(OpCodes.Tail)
        thunkIl.Emit(OpCodes.Call, methodDecl)
        thunkIl.Emit(OpCodes.Ret)

        thunkDecl.Body.Optimize()

        // If this is method has a captures environment then add it as an instance
        // mmethod to the environmen type. If not then add it as a plain static
        // method to the program type.
        match getParentEnv ctx with
        | Some parent ->
            let parentTy = EnvUtils.getType parent
            parentTy.Methods.Add methodDecl
            parentTy.Methods.Add thunkDecl
        | _ ->
            ctx.ProgramTy.Methods.Add methodDecl
            ctx.ProgramTy.Methods.Add thunkDecl

        markAsCompilerGenerated ctx.Core thunkDecl
        methodDecl, thunkDecl

    /// Emit the body of a library definition
    and emitLibrary ctx name mangledName exports body =

        // Genreate a nominal type to contain the methods for this library.
        let libTy =
            TypeDefinition(
                ctx.ProgramTy.Namespace,
                mangledName,
                libraryTypeAttributes,
                ctx.Assm.MainModule.TypeSystem.Object
            )

        ctx.Assm.MainModule.Types.Add libTy
        libTy.Methods.Add <| createEmptyCtor ctx.Assm
        markWithLibraryName ctx.Core libTy name
        ctx.Libraries <- Map.add mangledName libTy ctx.Libraries

        let exports, reExports =
            exports
            |> Seq.fold
                (fun state (name, storage) ->
                    match storage with
                    | StorageRef.Global(mangled, item) ->
                        let (exports, reExports) = state

                        if mangled = mangledName then
                            match item with
                            | Field id -> ((name, id) :: exports, reExports)
                            | _ -> icef "unexpected export type %A" item
                        else
                            (exports, ((name, mangled, item) :: reExports))
                    | _ -> state)
                ([], [])

        reExports
        |> List.iter (fun (externName, libTypName, item) ->
            let externlibTy =
                Map.tryFind libTypName ctx.Libraries
                |> Option.defaultWith (fun () -> getExternType ctx libTypName)

            markAsReExport ctx.Core libTy externName externlibTy item)

        // Emit the body of the script to a separate method so that the `Eval`
        // module can call it directly
        let libEmitCtx =
            { ctx with
                IL = null
                ProgramTy = libTy
                NextLambda = 0
                Locals = []
                Parameters = []
                Exports = exports |> Map.ofSeq
                Environment = None
                ParentEnvironment = None
                ScopePrefix = "$ROOT" }

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
    let emitMainEpilogue (assm: AssemblyDefinition) (il: ILProcessor) =
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

module Compilation =
    open Feersum.CompilerServices.Syntax.Parse

    /// Emit a Bound Expression to .NET
    ///
    /// Creates an assembly and writes out the .NET interpretation of the
    /// given bound tree. This method is responsible for creating the root
    /// `LispProgram` type and preparting the emit context. The main work of
    /// lowering is done by `emitNamedLambda`.
    let emit options target (outputStream: Stream) (outputName: string) (symbolStream: Stream option) externTys bound =

        /// Collect external types so we can look up their fields later
        let externs =
            externTys
            |> Seq.map (fun (ty: TypeDefinition) -> (ty.FullName, ty))
            |> Map.ofSeq

        // Create an assembly with a nominal version to hold our code
        let name =
            let stem = Path.GetFileNameWithoutExtension(outputName)

            AssemblyNameDefinition(stem, options.Version |> Option.defaultValue (Version(1, 0)))

        use resolver = new DefaultAssemblyResolver()
        List.iter (resolver.AddSearchDirectory) target.FrameworkLibLocations
        let moduleParams = ModuleParameters()

        moduleParams.Kind <-
            if options.OutputType = OutputType.Exe then
                ModuleKind.Console
            else
                ModuleKind.Dll

        moduleParams.AssemblyResolver <- resolver

        let assm = AssemblyDefinition.CreateAssembly(name, outputName, moduleParams)

        /// Import the initialisers for the extern libraries
        let inits =
            externTys
            |> Seq.choose (fun ty ->
                Seq.tryFind (fun (m: MethodDefinition) -> m.Name = "$LibraryBody") ty.Methods
                |> Option.map (fun m -> (ty.Name, m |> assm.MainModule.ImportReference)))
            |> Map.ofSeq


        // Genreate a nominal type to contain the methods for this program.
        let progTy =
            TypeDefinition(name.Name, bound.MangledName, libraryTypeAttributes, assm.MainModule.TypeSystem.Object)

        assm.MainModule.Types.Add progTy
        progTy.Methods.Add <| createEmptyCtor assm

        let coreTypes = Builtins.importCore assm target

        markCompilationRelaxations coreTypes assm

        if symbolStream.IsSome then
            markAsDebuggable coreTypes assm

        // Emit the body of the script to a separate method so that the `Eval`
        // module can call it directly
        let rootEmitCtx =
            { IL = null
              DebugDocuments = Dictionary()
              ProgramTy = progTy
              Externs = externs
              Libraries = Map.add bound.MangledName progTy Map.empty
              Initialisers = inits
              Core = coreTypes
              NextLambda = 0
              Locals = []
              Exports = Map.empty
              Parameters = []
              EmitSymbols = symbolStream.IsSome
              Environment = None
              ParentEnvironment = None
              ScopePrefix = "$ROOT"
              Assm = assm }

        let bodyParams = BoundFormals.List([])

        let bodyMethod, _ = emitNamedLambda rootEmitCtx "$ScriptBody" bodyParams bound.Root

        if options.OutputType = OutputType.Exe then
            // The `Main` method is the entry point of the program. It calls
            // `$ScriptBody` and coerces the return value to an exit code.
            let mainMethod =
                MethodDefinition(
                    "Main",
                    MethodAttributes.Public ||| MethodAttributes.Static,
                    assm.MainModule.TypeSystem.Int32
                )

            mainMethod.Parameters.Add(ParameterDefinition(ArrayType(assm.MainModule.TypeSystem.String)))
            progTy.Methods.Add mainMethod
            assm.EntryPoint <- mainMethod
            let il = mainMethod.Body.GetILProcessor()

            il.Emit(OpCodes.Call, bodyMethod)
            emitMainEpilogue assm il
            markAsCompilerGenerated coreTypes mainMethod

        // Write our `Assembly` to the output stream now we are done.
        let mutable writerParams = WriterParameters()

        match symbolStream with
        | Some(stream) ->
            writerParams.SymbolStream <- stream
            writerParams.WriteSymbols <- true
            writerParams.SymbolWriterProvider <- PortablePdbWriterProvider()
        | None -> ()

        assm.Write(outputStream, writerParams)
        name

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
    let compile options outputStream outputName symbolStream (input: CompileInput) =
        let target =
            match options.FrameworkAssmPaths with
            | [] -> TargetResolve.fromCurrentRuntime
            | paths -> TargetResolve.fromFrameworkPaths paths

        let (refTys, allLibs) =
            options.References
            |> Seq.map (Builtins.loadReferencedSignatures)
            |> Seq.append (Seq.singleton <| Builtins.loadCoreSignatures target)
            |> Seq.fold (fun (tys, sigs) (aTys, aSigs) -> (List.append tys aTys, List.append sigs aSigs)) ([], [])

        let scope =
            if options.OutputType = OutputType.Script then
                Binder.scopeFromLibraries allLibs
            else
                Binder.emptyScope

        let progs =
            match input with
            | CompileInput.Program progs -> List.map (fun (doc, prog) -> SyntaxShim.transformProgram doc prog) progs
            | CompileInput.Script(doc, script) ->
                script.Body |> Option.toList |> List.map (SyntaxShim.transformExpr doc)

        let input =
            { Kind = LegacyNodeKind.Seq progs
              Location = TextLocation.Missing }

        let bound = Binder.bind scope allLibs input

        let assmName =
            if hasErrors bound.Diagnostics |> not then
                bound
                |> Lower.lower
                |> emit options target outputStream outputName symbolStream refTys
                |> Some
            else
                None

        { Diagnostics = bound.Diagnostics
          EmittedAssemblyName = assmName }

    /// Read a collection of Files and Compile
    ///
    /// Takes the `sources` to an input to read and compile. Compilation results
    /// are written to `output`.
    let compileFiles (options: CompilationOptions) (output: string) (sources: string list) =

        // Handle the case that the user has specified a path to a directory but
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
        let output =
            if String.IsNullOrWhiteSpace(Path.GetFileName(output)) then
                Path.ChangeExtension(
                    Path.Join(outDir, Path.GetFileName(sources |> List.last)),
                    options.DefaultExtension
                )
            else
                output

        let result =
            sources
            |> Seq.map (fun path ->
                let contents = File.ReadAllText(path)
                let doc = TextDocument.fromParts path contents
                Parse.readProgram path contents |> ParseResult.map (fun r -> doc, r))
            |> ParseResult.fold (fun (progs) (p) -> List.append progs [ p ]) []

        if Diagnostics.hasErrors result.Diagnostics then
            result.Diagnostics
        else

            // Open the output streams. We don't use an `Option` directly here for
            // the symbols stream so we can drop it with `use`.
            use outputStream = File.OpenWrite output

            use symbols =
                match options.Configuration with
                | BuildConfiguration.Debug -> File.OpenWrite(Path.ChangeExtension(output, "pdb")) :> Stream
                // make a symbol file
                | BuildConfiguration.Release -> null

            let result =
                compile
                    options
                    outputStream
                    (Path.GetFileName(output))
                    (symbols |> Option.ofObj)
                    (CompileInput.Program result.Root)

            if result.Diagnostics.IsEmpty && options.OutputType = OutputType.Exe then
                match result.EmittedAssemblyName with
                | Some(assemblyName) -> Runtime.writeRuntimeConfig options output assemblyName outDir
                | None -> ()

            result.Diagnostics

    /// Read a File and Compile
    ///
    /// Takes the `source` to an input to read and compile. Compilation results
    /// are written to `output`.
    let compileFile options output source = compileFiles options output [ source ]
