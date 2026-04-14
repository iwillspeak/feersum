namespace Feersum.CompilerServices.Binding

open Feersum.CompilerServices.Binding
open Feersum.CompilerServices.Binding.Libraries

/// Shim type until proper scope is defined
type Scope = Map<string, StorageRef>

/// Creates initial binding environments for the expander.
///
/// FIXME: This module contains an odd mix of environment utitlities, syntax environment definitions and binding
///        environments. Really this all needs cleaning up as part of the 'Symbols API' refactor.
module Environments =

    /// Create a preloaded scope from a sequence of library signatures.
    /// Returns a Map<string, StorageRef> containing all exported names from the libraries.
    let fromLibraries (libs: seq<LibrarySignature<StorageRef>>) : Scope =
        libs |> Seq.collect (fun lib -> lib.Exports) |> Map.ofSeq

    /// The empty preloaded scope.
    let empty: Scope = Map.empty

    /// The empty initial scope. Special forms are not stored in the scope;
    /// they are recognised by name in `resolveHead` via `tryResolveSpecial`.
    /// Callers extend this with macro and variable bindings before expansion.
    let emptyStx: StxEnvironment = Map.empty

    /// Break open a Scope into Constituent Parts
    ///
    /// Internally a scope contains a both a mapping from names to their syntax
    /// binding, and a mapping from variable identifiers to their assigned
    /// storage locations.  This Function walks the scope and produces these two
    /// separate maps.
    let intoParts (scope: Scope) : StxEnvironment * Map<Ident, StorageRef> =
        scope
        |> Map.fold
            (fun (stx, env) key value ->
                let id = Ident.fresh ()
                (Map.add key (StxBinding.Variable(id)) stx, Map.add id value env))
            (Map.empty, Map.empty)

    /// Reconstruct a Scope from its Constituent Parts
    ///
    /// This is the inverse of `intoParts`. It takes a syntax environment and a
    /// variable environment and produces a Scope. This is a lossy operation.
    /// The returned `Scope` contains only syntax bindings which are live in
    /// the given `env`. Other bindings will be silently dropped.
    let fromParts (stx: StxEnvironment, env: Map<Ident, StorageRef>) : Scope =
        stx
        |> Map.fold
            (fun scope key binding ->
                match binding with
                | StxBinding.Variable id ->
                    match env.TryFind id with
                    | Some value -> Map.add key value scope
                    | None -> scope
                | _ -> scope)
            Map.empty
