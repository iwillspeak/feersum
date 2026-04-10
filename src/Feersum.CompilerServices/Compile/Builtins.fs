namespace Feersum.CompilerServices.Compile

open Mono.Cecil
open Mono.Cecil.Rocks

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
      TypeTy: TypeReference
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
      LispReExport: MethodReference
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

        let unpackStringArg (attr: CustomAttribute) idx =
            let arg = attr.ConstructorArguments[idx]

            if arg.Type = ty.Module.TypeSystem.String then
                arg.Value.ToString()
            else
                icef "Attempt to unpack arg %d as string, but found %A" idx arg.Type

        let unpackBoolArg (attr: CustomAttribute) idx =
            let arg = attr.ConstructorArguments[idx]

            match arg.Value with
            | :? Boolean as b -> b
            | _ -> icef "Attempt to unpack arg %d as string, but found %A" idx arg.Type

        let findExported onMatching (things: seq<'a> when 'a :> ICustomAttributeProvider) =
            things
            |> Seq.choose (fun thing ->
                thing.CustomAttributes
                |> Seq.tryPick (fun attr ->
                    if attr.AttributeType.Name = "LispExportAttribute" then
                        Some(((unpackStringArg attr 0), Global(ty.FullName, (onMatching thing))))
                    else
                        None))

        let exports = ty.Fields |> findExported (fun x -> Field(x.Name))

        let builtins = ty.Methods |> findExported (fun x -> Method(x.Name))

        let reExports =
            ty.CustomAttributes
            |> Seq.choose (fun attr ->
                if attr.AttributeType.Name = "LispReExportAttribute" then
                    let exportedItem =
                        let id = unpackStringArg attr 2

                        if unpackBoolArg attr 3 then Method(id) else Field(id)

                    let libTy = attr.ConstructorArguments[1].Value :?> TypeReference

                    Some((unpackStringArg attr 0, Global(libTy.FullName, exportedItem)))
                else
                    None)

        Seq.concat [ exports; builtins; reExports ] |> List.ofSeq

    /// Try to convert a given type definition into a library signature.
    let tryGetSignatureFromType (ty: TypeDefinition) =
        ty.CustomAttributes
        |> Seq.tryPick (fun attr ->
            if attr.AttributeType.Name = "LispLibraryAttribute" then
                Some(attr.ConstructorArguments[0].Value :?> CustomAttributeArgument[])
            else
                None)
        |> Option.map (fun name ->
            (ty,
             { LibraryName = name |> Seq.map (fun a -> a.Value.ToString()) |> List.ofSeq
               Exports = getExports ty }))

// --------------------  Builtin Macro Definitions -----------------------------

module private BuiltinMacros =
    open Feersum.CompilerServices.Text
    open Feersum.CompilerServices.NewBindingTest

    // -- Source strings (shared between old and new parsers) ------------------

    let private macroAndSrc =
        "(syntax-rules ::: ()
            ((_ a) a)
            ((_ a b :::) (if a (and b :::) #f))
            ((_) #t))"

    // TODO: re-write without the GUID trick once proper hygiene is in place.
    let private macroOrSrc =
        "(syntax-rules ()
            ((or) #f)
            ((or test) test)
            ((or test1 test2 ...)
                (let ((|90a3b246-0d7b-4f47-8e1e-0a9f0e7e3288| test1))
                    (if |90a3b246-0d7b-4f47-8e1e-0a9f0e7e3288| |90a3b246-0d7b-4f47-8e1e-0a9f0e7e3288| (or test2 ...)))))"

    let private macroWhenSrc =
        "(syntax-rules ()
            ((_ cond expr expr1 ...)
             (if cond
                (begin
                    expr
                    expr1 ...))))"

    let private macroUnlessSrc =
        "(syntax-rules ()
            ((_ cond expr expr1 ...)
             (if cond
                (if #f #f) ; FIXME: Hack to deal with not being able to see `not`
                (begin
                    expr
                    expr1 ...))))"

    let private macroCondSrc =
        "(syntax-rules (else =>)
            ((cond (else result1 result2 ...))
            (begin result1 result2 ...))
            ((cond (test => result))
            (let ((temp test))
            (if temp (result temp))))
            ((cond (test => result) clause1 clause2 ...)
            (let ((temp test))
            (if temp
                (result temp)
                (cond clause1 clause2 ...))))
            ((cond (test)) test)
            ((cond (test) clause1 clause2 ...)
            (let ((temp test))
            (if temp
                temp
                (cond clause1 clause2 ...))))
            ((cond (test result1 result2 ...))
            (if test (begin result1 result2 ...)))
            ((cond (test result1 result2 ...)
                clause1 clause2 ...)
            (if test
                (begin result1 result2 ...)
                (cond clause1 clause2 ...))))"

    // TODO: Can't support the `case` macro yet. This is the same issue as with
    //       the `unless` macro where we can't see `memv` to implement the pipe
    //       form. We need a better default environment to laod these macros in
    //       now that hygiene stops them from seeing items in the user's scope.


    // ── Old-format parser (Macros.parseSyntaxRules) ────────────────────────

    let private parseBuiltinMacro id rules =
        let registry = SourceRegistry.empty ()
        let result = Parse.readExpr1 registry (sprintf "builtin-%s" id) rules

        if hasErrors result.Diagnostics then
            icef "Error in builtin macro: %A" result.Diagnostics

        match result.Root.Body with
        | Some expr -> MacrosOld.parseSyntaxRules id expr |> Result.unwrap
        | None -> icef "no body in builtin macro %A" result.Root.Text

    let private macroAnd = macroAndSrc |> parseBuiltinMacro "and"
    let private macroOr = macroOrSrc |> parseBuiltinMacro "or"
    let private macroWhen = macroWhenSrc |> parseBuiltinMacro "when"
    let private macroUnless = macroUnlessSrc |> parseBuiltinMacro "unless"
    let private macroCond = macroCondSrc |> parseBuiltinMacro "cond"

    /// The list of builtin macros (old StorageRef.Macro format, for Binder/Compiler).
    let coreMacros =
        { LibraryName = [ "feersum"; "builtin"; "macros" ]
          Exports =
            [ macroAnd; macroOr; macroWhen; macroUnless; macroCond ]
            |> List.map (fun m -> (m.Name, StorageRef.Macro(m))) }

    // ── New-format loader ──────────────────────────────────────────────────

    /// Load all builtin macros (`and`, `or`, `when`, `unless`, `cond`) into a
    /// `StxEnvironment`, starting from `Map.empty`.
    ///
    /// Special forms (`if`, `let`, `begin`, etc.) are recognised by name in
    /// `resolveHead` and do not need scope entries.
    ///
    /// For each macro:
    ///  - a fresh Ident is reserved so the macro can see its own name in defScope
    ///    (enabling self-recursive macros like `and` and `cond`);
    ///  - the syntax rules are parsed against that extended scope;
    ///  - the transformer is registered in `ctx.MacroRegistry`.
    let loadBuiltinMacroEnv (ctx: ExpandCtx) : StxEnvironment =
        let macros =
            [ "and", macroAndSrc
              "or", macroOrSrc
              "when", macroWhenSrc
              "unless", macroUnlessSrc
              "cond", macroCondSrc ]

        macros
        |> List.fold
            (fun scope (name, src) ->
                let registry = SourceRegistry.empty ()
                let result = Parse.readExpr1 registry (sprintf "builtin-new-%s" name) src

                if hasErrors result.Diagnostics then
                    icef "Error parsing new-format builtin macro '%s': %A" name result.Diagnostics

                match result.Root.Body with
                | None -> icef "no body in new-format builtin macro '%s'" name
                | Some expr ->
                    let diags = DiagnosticBag.Empty
                    let stx = Stx.ofExpr registry result.Root.DocId diags expr

                    // Reserve an Ident first so the macro sees its own name in defScope;
                    // all previously-accumulated macros are also visible.
                    let id, scope' = ExpandCtx.reserveMacro name scope

                    match New.Macros.makeSyntaxTransformer name stx scope' diags with
                    | Some transformer ->
                        ExpandCtx.registerMacro ctx id transformer
                        scope'
                    | None -> icef "Failed to parse new-format builtin macro '%s': %A" name diags.Diagnostics)
            Map.empty

// ------------------------ Public Builtins API --------------------------------

module Builtins =
    open Feersum.CompilerServices.NewBindingTest

    /// Load all builtin macros (`and`, `or`, `when`, `unless`, `cond`) into the
    /// initial `StxEnvironment`, starting from `Map.empty`.  Special forms are
    /// recognised by name in `resolveHead` and do not need scope entries.  Each
    /// macro Ident is reserved and its transformer registered in `ctx.MacroRegistry`
    /// so the result can be passed directly to `Expand.expandProgram`.
    let loadBuiltinMacroEnv (ctx: ExpandCtx) : StxEnvironment = BuiltinMacros.loadBuiltinMacroEnv ctx


    /// Scan the `externAssms` and retrieve the core types that are required to
    /// compile a scheme progrma. These `CoreTypes` represent the types and methods
    /// that the compilation will emit calls to as part of code generation and
    /// intrisics.
    let private loadCoreTypes (lispAssm: AssemblyDefinition) (externAssms: seq<AssemblyDefinition>) =

        // Materialize the sequence once to avoid multiple enumeration of a potentially non-replayable seq.
        let externAssmsArray = externAssms |> Seq.toArray

        let loadedAssemblies =
            lazy
                (externAssmsArray
                 |> Seq.map (fun a -> sprintf "  - %s" a.Name.Name)
                 |> String.concat "\n")

        let getType name =
            externAssmsArray
            |> Seq.tryPick (fun (assm: AssemblyDefinition) ->
                // Check both direct types and exported types (which includes forwarded types)
                assm.MainModule.Types
                |> Seq.tryFind (fun x -> x.FullName = name)
                |> Option.orElseWith (fun () ->
                    assm.MainModule.ExportedTypes
                    |> Seq.tryPick (fun x -> if x.FullName = name then Some(x.Resolve()) else None)))
            |> Option.defaultWith (fun () ->
                let loadedAssms = loadedAssemblies.Force()

                icef "Failed to find type '%s' in loaded assemblies:\n%s" name loadedAssms)

        let getImportedType name =
            getType name |> lispAssm.MainModule.ImportReference

        let getResolvedType name = (getImportedType name).Resolve()

        let getCtorBy pred typeName =
            let ty = getResolvedType typeName

            ty.GetConstructors()
            |> Seq.tryFind pred
            |> Option.defaultWith (fun () -> icef "No constructor matching predicate found on type '%s'" typeName)
            |> lispAssm.MainModule.ImportReference

        let getSingleCtor = getCtorBy (fun _ -> true)

        let getCtorByArity arity =
            getCtorBy (fun m -> m.Parameters.Count = arity)

        // Func<object[], object> is akward because we have to convert both the
        // constructor and invoke methods into instnace methods on the correctly
        // bound generic instance.
        let objTy = lispAssm.MainModule.TypeSystem.Object

        let genericArgs = [| objTy.MakeArrayType() :> TypeReference; objTy |]

        let funcTy =
            (getType "System.Func`2").MakeGenericInstanceType(genericArgs).Resolve()

        let funcCtor =
            funcTy.GetConstructors()
            |> Seq.toArray
            |> function
                | [||] -> icef "No constructor found on System.Func`2"
                | ctors -> ctors[0]
            |> lispAssm.MainModule.ImportReference
            |> makeHostInstanceGeneric genericArgs

        let funcInvoke =
            funcTy.GetMethods()
            |> Seq.tryFind (fun m -> m.Name = "Invoke")
            |> Option.defaultWith (fun () -> icef "Invoke method not found on System.Func`2")
            |> lispAssm.MainModule.ImportReference
            |> makeHostInstanceGeneric genericArgs

        let getMethod typeName methodName =
            let ty = getResolvedType typeName

            ty.GetMethods()
            |> Seq.tryFind (fun m -> m.Name = methodName)
            |> Option.defaultWith (fun () -> icef "Method '%s' not found on type '%s'" methodName typeName)
            |> lispAssm.MainModule.ImportReference

        { ConsTy = getImportedType "Serehfa.ConsPair"
          ValueType = getImportedType "System.ValueType"
          TypeTy = getImportedType "System.Type"
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
          LispReExport = getSingleCtor "Serehfa.Attributes.LispReExportAttribute"
          LispLibrary = getSingleCtor "Serehfa.Attributes.LispLibraryAttribute"
          AssmConfigCtor = getSingleCtor "System.Reflection.AssemblyConfigurationAttribute"
          CompRelaxAttr = getSingleCtor "System.Runtime.CompilerServices.CompilationRelaxationsAttribute"
          CompRelaxations = getImportedType "System.Runtime.CompilerServices.CompilationRelaxations" }

    /// Load the signature from a given libary name
    let loadReferencedSignatures (name: string) =
        /// Folds a sequence of references into a single pair of lists
        let combineSignatures sigs =
            sigs |> Seq.fold (fun (tys, sigs) (t, s) -> (t :: tys, s :: sigs)) ([], [])

        use assm = Mono.Cecil.AssemblyDefinition.ReadAssembly(name, assmReadParams)

        assm.MainModule.Types |> Seq.choose tryGetSignatureFromType |> combineSignatures

    /// The core library signature
    let loadCoreSignatures target =
        let (tys, sigs) = loadReferencedSignatures target.LispCoreLocation

        let sigs = BuiltinMacros.coreMacros :: sigs

        (tys, sigs)

    /// Load the core types into the given assembly
    let importCore (targetAssm: AssemblyDefinition) target =

        let coreAssemblies =
            target.LispCoreLocation :: target.FrameworkLibLocations
            |> List.map (fun x -> AssemblyDefinition.ReadAssembly(x, assmReadParams))

        try
            loadCoreTypes targetAssm coreAssemblies
        finally
            coreAssemblies |> List.iter (fun assm -> (assm :> IDisposable).Dispose())

    /// Load the assembly and retrieve the name from it.
    let getAssemblyName (path: string) =
        use assm = Mono.Cecil.AssemblyDefinition.ReadAssembly(path, assmReadParams)

        assm.Name
