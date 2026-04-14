namespace Feersum.CompilerServices.Binding.New

open Feersum.CompilerServices.Binding.Stx
open Feersum.CompilerServices.Binding
open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Text
open Feersum.CompilerServices.Syntax
open Feersum.CompilerServices.Syntax.Tree
open Feersum.CompilerServices.Ice

module private BinderDiagnostics =

    let patternBindError =
        DiagnosticKind.Create DiagnosticLevel.Error 30 "Invalid or incomplete pattern syntax"

    let missingExport =
        DiagnosticKind.Create DiagnosticLevel.Warning 31 "Missing export"

    let illFormedSpecialForm =
        DiagnosticKind.Create DiagnosticLevel.Error 32 "Ill-formed special form"

    let invalidParameterPattern =
        DiagnosticKind.Create DiagnosticLevel.Error 33 "Invalid formal parameter pattern"

    let letBindError =
        DiagnosticKind.Create DiagnosticLevel.Error 34 "Invalid let binding"

    let undefinedSymbol =
        DiagnosticKind.Create DiagnosticLevel.Error 35 "Reference to undefined symbol"

    let invalidImport =
        DiagnosticKind.Create DiagnosticLevel.Error 36 "Invalid import declaration"

    let uninitialisedVariale =
        DiagnosticKind.Create DiagnosticLevel.Error 37 "Use of uninitialised variable"

    let malformedDatum =
        DiagnosticKind.Create DiagnosticLevel.Error 38 "Invalid datum value"

// -- Global Binder Context ----------------------------------------------------

/// Binder Context Type
///
/// A binder context holds the shared state for binding a set of units in a
/// given bind pass. This is effectively the "environment" for the binder and
/// contains a the accumulated global state for the current binding pass.
type private BinderCtx =
    { Diagnostics: DiagnosticBag
      mutable Libraries: LibrarySignature<StorageRef> list
      SourceRegistry: SourceRegistry }

module private Binderctx =

    /// Create a new BinderCtx with the given scope and name
    ///
    /// Seeds the scope from the given `scope`.
    let create sourceRegistry libs =
        { SourceRegistry = sourceRegistry
          Libraries = libs
          Diagnostics = DiagnosticBag.Empty }

/// A single frame in the scope stack of a given binding frame. This is used to
/// record the resolved storage locations of values which are currently
/// accessable in the given context.
type ResolutionScope = Map<Ident, StorageRef>

/// The full resolution enviroment for a given frame. A stakc of scopes where
/// the head is the innermost scope and the tail is the outermost. Resolution
/// walks the scopes until an identifier is found or, if no binding resolution
/// is found, the outer frame should be searched.
type private ResolutionEnv = S of ResolutionScope list

module private ResolutionEnv =

    /// An empty resolution environment. This contains a single scope without
    /// any bindings.
    let empty = S [ Map.empty ]

    /// Push a new scope onto the resolution environment. This is used when
    /// entering a new `let` body or similar.
    let pushScope (S scopes) = S(Map.empty :: scopes)

    /// Pop a scope from the resolution environment. Used when leaving a scope
    /// body to remove bindings from the resolution environment.
    let popScope (S scopes) =
        match scopes with
        | [] -> ice "ResolutionEnv.popScope: no scopes to pop"
        | _ :: rest -> S rest

    /// Try to resolve an identifier in the resolution environment. This walks
    /// the scopes from innermost to outermost and returns the first binding
    /// found for the given identifier, if any.
    let tryResolve (S scopes) (ident: Ident) : StorageRef option =
        scopes |> List.tryPick (Map.tryFind ident)

    /// Extend the given scope with a new binding `ident` -> `var`
    let extend (S scopes) (ident: Ident) (var: StorageRef) =
        match scopes with
        | active :: tail -> S(Map.add ident var active :: tail)
        | _ -> ice "ResolutionEnvironment has no active scope"

/// Binder Frame Context
///
/// A frame context represents the state of the binder as it processes a
/// particular item. This includes the kind of item being processed and the
/// "resolution contet", a map from source identifiers to storage.
type private FrameCtx =
    { Kind: FrameKind
      BinderCtx: BinderCtx
      mutable Env: ResolutionEnv
      mutable LocalCount: int }

/// State specific to the kind of frame being bound. This reprsesents the
/// different types of items we can process.
and private FrameKind =
    /// Root frame. This represents a top level script o program frame being
    /// bound into the type with the given `mangledName`.
    | Global of mangledName: string

    /// Libraries behave similarly to a Global frame, but also have a parent
    /// frame which they can refer to for name resolution.
    | Library of mangledName: string * parent: FrameCtx

    /// Lamdba frames have mutable capture state which is used to track the
    /// variables captured from outer frames.
    | Lambda of LambdaState

/// Lamdba specific frame state. Needed because we must mutate the captures
/// as we process the body of the lambda so this can't live directly on the
/// `FrameKind.Lambda` case.
and private LambdaState =
    {
        Parent: FrameCtx
        /// Captures state of the lambda. If `None` then no environment captures
        /// exist. If `Some` then the list shows the captured variables.
        ///
        /// A `Some []` indicates a link environment.
        mutable Captures: StorageRef list option
    }


/// A helper module for working with `FrameCtx`. This contains functions for
/// creating and manipulating `FrameCtx` values, such as pushing and popping
/// frames and managing the resolution environment.
module private FrameCtx =

    let private createWithKind binderCtx kind =
        { Kind = kind
          BinderCtx = binderCtx
          Env = ResolutionEnv.empty
          LocalCount = 0 }

    /// Create a new `FrameCtx` for a global frame with the given `mangledName`.
    let createGlobal binderCtx mangledName scope =
        createWithKind binderCtx (Global mangledName)
        |> fun ctx -> { ctx with Env = S [ scope ] }

    /// Create a new `FrameCtx` for a library frame with the given `mangledName`
    /// and `parent` frame.
    let createLibrary binderCtx mangledName parent =
        createWithKind binderCtx (Library(mangledName, parent))

    /// Create a new `FrameCtx` for a lambda frame with the given parent frame.
    let createLambda binderCtx parent =
        createWithKind binderCtx (Lambda { Parent = parent; Captures = None })

    /// Resolve an identifier in the given `FrameCtx`.
    ///
    /// This first tries to resolve the identifier in the current frame's
    /// resolution environment. If that fails, it then tries to resolve the
    /// identifier in the parent frame, if one exists. If the identifier is
    /// found in the parent frame, it is registered as a capture on the current
    /// frame if the parent storage is a local, argument, captured variable, or
    /// environment variable. If the identifier is not found in the parent
    /// frame, then `None` is returned to indicate an unresolved identifier.
    let rec resolve (ctx: FrameCtx) (ident: Ident) : StorageRef option =
        match ResolutionEnv.tryResolve ctx.Env ident with
        | Some storage -> Some storage
        | None ->
            // Not in this frame's scopes. Try the parent (captures).
            match ctx.Kind with
            | Lambda state ->
                match resolve state.Parent ident with
                | Some outerStorage ->
                    // Register capture on this lambda frame.
                    match outerStorage with
                    | Captured _
                    | Arg _
                    | Local _
                    | Environment _ ->
                        state.Captures <-
                            match state.Captures with
                            | Some captures -> Some(outerStorage :: captures)
                            | None -> Some [ outerStorage ]

                        Some(StorageRef.Captured outerStorage)
                    | _ -> Some outerStorage
                | None -> None
            | Library(_, parent) -> resolve parent ident
            | Global _ ->
                // Global frames have no parent to search.
                None

    /// Finish a `FrameCtx` by producing a `BoundBody` with the given `body`
    /// expression and capture information resolved from the `ctx`'s state.
    let finish (ctx: FrameCtx) (body: BoundExpr) : BoundBody =
        let captures, env =
            match ctx.Kind with
            | Lambda state ->
                match state.Captures with
                | Some captures -> captures, Some([])
                | None -> [], None
            | _ -> [], None

        { Body = body
          Locals = ctx.LocalCount
          Captures = captures
          EnvMappings = env }

// -- Binder Implementation ----------------------------------------------------

[<AutoOpen>]
module private Impl =

    open System.Text.RegularExpressions

    /// Create a "Mangled" Name for a Given Source name
    ///
    /// This converts a sequecne of name parts, such as those found in a library
    /// or module in Scheme into a single CIL identifier safe string.
    let mangleName (name: string seq) : string =
        let mangleNamePart (part: string) =
            Regex.Replace(part, @"[^a-zA-Z0-9]+", "_")

        name |> Seq.map mangleNamePart |> String.concat "::"

    // Get the Syntax Location for a Given Syntax Node
    let private getSyntaxLocation (_ctx: BinderCtx) (stx: Stx) : TextLocation =
        // FIXME: We _should_ have to look the stx up in the source registry
        //        so we can `rangeToLocation` the span. But the current slop
        //        means the locations are actually just denormalised on each stx
        stx.Loc

    /// Bind a Top Level Item
    ///
    /// Top level items can be commands or definitions
    let bindTopLevel (ctx: FrameCtx) (syntaxEnv: StxEnvironment) (item: Stx) : BoundExpr * StxEnvironment =
        unimpl "Binder.bindTopLevel: not implemented yet"

    /// Bind the Import Declarations in a Program
    ///
    /// Updates the `StxEnvironment` with the imported bindings and returns the
    /// remaining body of the program to be bound.
    let bindImports (ctx: FrameCtx) (stxEnv: StxEnvironment) (stxs: Stx list) : Stx list * StxEnvironment =
        let rec addImportsToEnv (stxEnv: StxEnvironment) (stx: Stx) : StxEnvironment =
            let imp = Libraries.parseImport ctx.BinderCtx.Diagnostics stx

            match Libraries.resolveImport ctx.BinderCtx.Libraries imp with
            | Ok library ->
                let newScope =
                    library.Exports
                    |> List.fold
                        (fun s (name, storage) ->
                            let id = Ident.fresh ()
                            ctx.Env <- ResolutionEnv.extend ctx.Env id storage
                            Map.add name (StxBinding.Variable id) s)
                        stxEnv

                newScope
            | Result.Error msg ->
                ctx.BinderCtx.Diagnostics.Emit BinderDiagnostics.invalidImport stx.Loc msg
                stxEnv

        let rec loop stxEnv remainingStx =
            match remainingStx with
            | StxList(StxId(id, _, envInner) :: args, None, _, envOuter) :: rest ->
                let envOuter = Option.defaultValue stxEnv envOuter
                let envInner = Option.defaultValue envOuter envInner
                let resolved = Stx.resolve id envInner

                match resolved with
                | Some(StxBinding.Special SpecialFormKind.Import) ->
                    let stxEnv = args |> List.fold addImportsToEnv stxEnv
                    loop stxEnv rest
                | _ ->
                    // Not an import, stop processing and return the remaining stx
                    remainingStx, stxEnv
            | _ -> remainingStx, stxEnv

        loop stxEnv stxs


    /// Bind a Program Root
    ///
    /// Walks the list of items and binds each one as a top level item
    let bindProgramRoot (ctx: FrameCtx) (stxEnv: StxEnvironment) (stxs: Stx list) : BoundBody =
        let stxs, stxEnv = bindImports ctx stxEnv stxs

        let boundItems = stxs |> List.mapFold (bindTopLevel ctx) stxEnv |> fst

        { Body = Seq(List.ofSeq boundItems)
          Locals = ctx.LocalCount
          Captures = []
          EnvMappings = None }


/// Public Binder API
///
/// This module contains the user-facing API for the Binder. Users call
/// `Binder.bind` to convera a sequence of `Expression` representgin the files
/// within a program or script into a `BoundExpr` which can then be passed to
/// `Lower` and `Emit`.
module Binder =

    /// Bind a List of Syntax Nodes in a Given Scope
    ///
    /// This is the main entry point for the binder. It takes a list of syntax
    /// nodes representing the program to be bound, along with a source registry
    /// and a scope for name resolution, and produces a `BoundSyntaxTree` which
    /// contains the bound syntax tree along with diagnostics and a mangled name.
    let private bindStx
        (ctx: BinderCtx)
        (stxEnv: StxEnvironment)
        (scope: Map<Ident, StorageRef>)
        (stx: Stx list)
        : BoundSyntaxTree =
        let name = "LispProgram"
        let rootFrame = FrameCtx.createGlobal ctx name scope

        let bound = bindProgramRoot rootFrame stxEnv stx

        { Root = bound
          MangledName = name
          Diagnostics = ctx.Diagnostics.Take }


    /// Bind a List of Compilation Units in a Given Scope
    ///
    /// Takes a list of dcocument and expression pairs and binds them in the
    /// given scope.
    let bindProgram
        (sourceRegistry: SourceRegistry)
        (stxEnv: StxEnvironment)
        (scope: Map<Ident, StorageRef>)
        (libs: seq<LibrarySignature<StorageRef>>)
        (units: Tree.Program list)
        : BoundSyntaxTree =
        let ctx = Binderctx.create sourceRegistry (List.ofSeq libs)

        let toBind =
            units
            |> Seq.collect (fun unit -> unit.Body |> Seq.map (Stx.ofExpr sourceRegistry unit.DocId ctx.Diagnostics))

        bindStx ctx stxEnv scope (List.ofSeq toBind)
