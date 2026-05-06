namespace Feersum.CompilerServices.Binding

open Feersum.CompilerServices.Binding.Libraries

/// The compilation scope.
///
/// Bundles all environment pieces needed by the binder for a single
/// compilation pass: the syntax environment (macros and variable bindings),
/// the storage bindings for each resolved identifier, the available library
/// signatures for import resolution, and the registered macro transformers.
[<NoComparison; NoEquality>]
type Scope =
    { StxEnv: StxEnvironment
      Bindings: Map<Ident, StorageRef>
      Libs: LibrarySignature<StorageRef> list
      Macros: Map<Ident, SyntaxTransformer>
      ProgramName: string }

module Scope =

    /// The empty scope with no bindings, libraries, or macros.
    let empty: Scope =
        { StxEnv = Map.empty
          Bindings = Map.empty
          Libs = []
          Macros = Map.empty
          ProgramName = "LispProgram" }

    /// Build a scope from a sequence of library signatures.
    ///
    /// Each exported name gets a fresh `Ident` in the syntax environment and
    /// its `StorageRef` in the binding environment. The libraries are retained
    /// so the binder can resolve subsequent `(import ...)` forms.
    let ofLibraries (libs: seq<LibrarySignature<StorageRef>>) : Scope =
        let stxEnv, bindings =
            libs
            |> Seq.collect (fun l -> l.Exports)
            |> Seq.fold
                (fun (stx, env) (name, storage) ->
                    let id = Ident.fresh ()
                    Map.add name (StxBinding.Variable id) stx, Map.add id storage env)
                (Map.empty, Map.empty)

        { StxEnv = stxEnv
          Bindings = bindings
          Libs = List.ofSeq libs
          Macros = Map.empty
          ProgramName = "LispProgram" }

/// Creates initial binding environments for the expander.
module Environments =

    /// The empty initial syntax environment. Special forms are not stored here;
    /// they are recognised by name in `resolveHead` via `tryResolveSpecial`.
    let emptyStx: StxEnvironment = Map.empty
