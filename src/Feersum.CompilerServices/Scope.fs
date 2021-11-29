namespace Feersum.CompilerServices

/// An entry in the current scope.
type ScopeEntry<'t> = { mutable Value: 't; Id: string }

/// The scope type. Used to store locals by the binder.
type Scope<'t> = { Entries: ScopeEntry<'t> list }

module Scope =

    /// Create a new empty scope.
    let empty = { Entries = [] }

    /// Get an entry in the scope. This allows the value stored in the scope to be
    /// modified if needed.
    let entry scope id =
        List.tryFind (fun e -> e.Id = id) scope.Entries

    /// Find an entry in the scope.
    let find scope id =
        entry scope id |> Option.map (fun x -> x.Value)

    /// Add an entry to the given scope. Returns a new scope with the given entry
    /// added at the current depth.
    let insert scope id value =
        let entry = { Value = value; Id = id }

        { scope with Entries = entry :: scope.Entries }

    /// Create a scope from the initial environment map
    let fromMap map = Map.fold insert empty map
