namespace Feersum.CompilerServices.Compile

open Mono.Cecil
open Mono.Cecil.Rocks
open Mono.Cecil.Cil

open Feersum.CompilerServices.Ice
open Feersum.CompilerServices.Compile.MonoHelpers
open System
open Feersum.CompilerServices.Syntax
open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Utils
open Feersum.CompilerServices.Binding
open Feersum.CompilerServices.Targets

/// Core Types Used by the Compiler at Runtime
type CoreTypes =
    { ConsTy: TypeReference
      ValueType: TypeReference
      EnvTy: TypeDefinition
      ConsCtor: MethodReference
      UndefinedInstance: MethodReference
      IdentCtor: MethodReference
      RuntimeInitArray: MethodReference
      FuncObjTy: TypeReference
      FuncObjCtor: MethodReference
      FuncObjInvoke: MethodReference
      ExceptionCtor: MethodReference
      DebuggableCtor: MethodReference
      CompGenCtor: MethodReference
      NonUserCodeCtor: MethodReference
      StepThroughCtor: MethodReference
      CompRelaxAttr: MethodReference
      CompRelaxations: TypeReference
      LispExport: MethodReference
      LispLibrary: MethodReference
      AssmConfigCtor: MethodReference }

// --------------------  External Reference Utils -----------------------------

[<AutoOpen>]
module private ExternUtils =

    /// Reder parameters for Mono assembly loading.
    let assmReadParams =
        let r = ReaderParameters()
        r.ReadSymbols <- false
        r

    /// Get Exported items from a given Mono type definition.
    let private getExports (ty: TypeDefinition) =

        let unpackStringArg (attr: CustomAttribute) =
            attr.ConstructorArguments.[0].Value.ToString()

        let chooseMatching name onMatching (things: seq<'a> when 'a :> ICustomAttributeProvider) =
            things
            |> Seq.choose
                (fun thing ->
                    thing.CustomAttributes
                    |> Seq.tryPick
                        (fun attr ->
                            if attr.AttributeType.Name = name then
                                Some(((unpackStringArg attr), Global(ty.FullName, (onMatching thing))))
                            else
                                None))

        let exports =
            ty.Fields
            |> chooseMatching "LispExportAttribute" (fun x -> Field(x.Name))

        let builtins =
            ty.Methods
            |> chooseMatching "LispBuiltinAttribute" (fun x -> Method(x.Name))

        Seq.append exports builtins |> List.ofSeq

    /// Try to convert a given type definition into a library signature.
    let tryGetSignatureFromType (ty: TypeDefinition) =
        ty.CustomAttributes
        |> Seq.tryPick
            (fun attr ->
                if attr.AttributeType.Name = "LispLibraryAttribute" then
                    Some(attr.ConstructorArguments.[0].Value :?> CustomAttributeArgument [])
                else
                    None)
        |> Option.map
            (fun name ->
                (ty,
                 { LibraryName =
                       name
                       |> Seq.map (fun a -> a.Value.ToString())
                       |> List.ofSeq
                   Exports = getExports ty }))

// --------------------  Builtin Macro Definitions -----------------------------

module private BuiltinMacros =

    /// Parse a builtin macro from syntax rules
    let private parseBuiltinMacro id rules =
        let (node, errs) =
            Parse.readExpr1 (sprintf "builtin-%s" id) rules

        if hasErrors errs then
            icef "Error in builtin macro: %A" errs

        match node with
        | { Kind = AstNodeKind.Seq ([ n ]) } -> n
        | n -> n
        |> Macros.parseSyntaxRules id
        |> Result.unwrap


    /// Builtin `and` Macro
    let private macroAnd =
        "(syntax-rules ::: ()
            ((_ a) a)
            ((_ a b :::) (if a (and b :::) #f))
            ((_) #t))"
        |> parseBuiltinMacro "and"

    /// Builtin `or` Macro
    let private macroOr =
        // TODO: re-write this when proper hygene is supported.
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

    let private macroCond =
        // TODO: This `cond` implementation doesn't support the `=>` 'pipe' form
        //       of the macro yet. This is because we're waiting for hygene
        //       support like `or`.
        "(syntax-rules (else)
            ((cond (else e ...))   (begin e ...))
            ((cond (test e e1 ...))
                (if test
                    (begin e e1 ...)))
            ((cond (test e e1 ...) c ...)
                (if test
                    (begin e e1 ...)
                    (cond c ...))))"
        |> parseBuiltinMacro "cond"

    /// The list of builtin macros
    let coreMacros =
        { LibraryName = [ "scheme"; "base" ]
          Exports =
              [ macroAnd
                macroOr
                macroWhen
                macroUnless
                macroCond ]
              |> List.map (fun m -> (m.Name, StorageRef.Macro(m))) }

// ------------------------ Public Builtins API --------------------------------

module Builtins =

    /// Scan the `externAssms` and retrieve the core types that are required to
    /// compile a scheme progrma. These `CoreTypes` represent the types and methods
    /// that the compilation will emit calls to as part of code generation and
    /// intrisics.
    let private loadCoreTypes (lispAssm: AssemblyDefinition) (externAssms: seq<AssemblyDefinition>) =

        let getType name =
            externAssms
            |> Seq.pick
                (fun (assm: AssemblyDefinition) ->
                    assm.MainModule.Types
                    |> Seq.tryFind (fun x -> x.FullName = name))

        let getImportedType name =
            getType name
            |> lispAssm.MainModule.ImportReference

        let getResolvedType name = (getImportedType name).Resolve()

        let getCtorBy pred typeName =
            let ty = getResolvedType typeName

            ty.GetConstructors()
            |> Seq.find pred
            |> lispAssm.MainModule.ImportReference

        let getSingleCtor = getCtorBy (fun _ -> true)

        let getCtorByArity arity =
            getCtorBy (fun m -> m.Parameters.Count = arity)

        // Func<object[], object> is akward because we have to convert both the
        // constructor and invoke methods into instnace methods on the correctly
        // bound generic instance.
        let objTy = lispAssm.MainModule.TypeSystem.Object

        let genericArgs =
            [| objTy.MakeArrayType() :> TypeReference
               objTy |]

        let funcTy =
            (getType "System.Func`2")
                .MakeGenericInstanceType(genericArgs)
                .Resolve()

        let funcCtor =
            lispAssm.MainModule.ImportReference(funcTy.GetConstructors() |> Seq.head)
            |> makeHostInstanceGeneric genericArgs

        let funcInvoke =
            lispAssm.MainModule.ImportReference(
                funcTy.GetMethods()
                |> Seq.find (fun m -> m.Name = "Invoke")
            )
            |> makeHostInstanceGeneric genericArgs

        let getMethod typeName methodName =
            let ty = getResolvedType typeName

            ty.GetMethods()
            |> Seq.find (fun m -> m.Name = methodName)
            |> lispAssm.MainModule.ImportReference

        { ConsTy = getImportedType "Serehfa.ConsPair"
          ValueType = getImportedType "System.ValueType"
          ConsCtor = getSingleCtor "Serehfa.ConsPair"
          IdentCtor = getSingleCtor "Serehfa.Ident"
          RuntimeInitArray = getMethod "System.Runtime.CompilerServices.RuntimeHelpers" "InitializeArray"
          UndefinedInstance = getMethod "Serehfa.Undefined" "get_Instance"
          FuncObjTy =
              funcTy.MakeGenericInstanceType(genericArgs)
              |> lispAssm.MainModule.ImportReference
          FuncObjCtor = funcCtor
          FuncObjInvoke = funcInvoke
          ExceptionCtor = getCtorByArity 1 "System.Exception"
          DebuggableCtor = getCtorByArity 2 "System.Diagnostics.DebuggableAttribute"
          CompGenCtor = getSingleCtor "System.Runtime.CompilerServices.CompilerGeneratedAttribute"
          NonUserCodeCtor = getSingleCtor "System.Diagnostics.DebuggerNonUserCodeAttribute"
          StepThroughCtor = getSingleCtor "System.Diagnostics.DebuggerStepThroughAttribute"
          LispExport = getSingleCtor "Serehfa.Attributes.LispExportAttribute"
          LispLibrary = getSingleCtor "Serehfa.Attributes.LispLibraryAttribute"
          AssmConfigCtor = getSingleCtor "System.Reflection.AssemblyConfigurationAttribute"
          CompRelaxAttr = getSingleCtor "System.Runtime.CompilerServices.CompilationRelaxationsAttribute"
          CompRelaxations = getImportedType "System.Runtime.CompilerServices.CompilationRelaxations"
          EnvTy = addEnvDecls lispAssm }

    /// Load the signature from a given libary name
    let loadReferencedSignatures (name: string) =
        /// Folds a sequence of references into a single pair of lists
        let combineSignatures sigs =
            sigs
            |> Seq.fold (fun (tys, sigs) (t, s) -> (t :: tys, s :: sigs)) ([], [])

        use assm =
            Mono.Cecil.AssemblyDefinition.ReadAssembly(name, assmReadParams)

        assm.MainModule.Types
        |> Seq.choose tryGetSignatureFromType
        |> combineSignatures

    /// The core library signature
    let loadCoreSignatures target =
        let (tys, sigs) =
            loadReferencedSignatures target.LispCoreLocation

        let sigs =
            BuiltinMacros.coreMacros :: sigs
            |> Seq.groupBy (fun l -> l.LibraryName)
            |> Seq.map
                (fun (n, sigs) ->
                    { LibraryName = n
                      Exports =
                          Seq.collect (fun x -> x.Exports) sigs
                          |> List.ofSeq })

        (tys, sigs |> List.ofSeq)

    /// Load the core types into the given assembly
    let importCore (targetAssm: AssemblyDefinition) target =

        let coreAssemblies =
            target.LispCoreLocation
            :: target.MSCoreLibLocations
            |> List.map (fun x -> AssemblyDefinition.ReadAssembly(x, assmReadParams))

        try
            loadCoreTypes targetAssm coreAssemblies
        finally
            coreAssemblies
            |> List.iter (fun assm -> (assm :> IDisposable).Dispose())

    /// Load the assembly and retrieve the name from it.
    let getAssemblyName (path: string) =
        use assm =
            Mono.Cecil.AssemblyDefinition.ReadAssembly(path, assmReadParams)

        assm.Name
