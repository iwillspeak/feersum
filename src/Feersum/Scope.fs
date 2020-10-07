module Scope

/// An entry in the current scope.
type ScopeEntry<'t> = { mutable Value: 't; Depth: int; Id: string }

/// The scope type. Used to store locals by the binder.
type Scope<'t> = { Entries: ScopeEntry<'t> list
                 ; CurrentDepth: int }

/// Create a new empty scope.
let public empty =
    { Entries = []; CurrentDepth = 0 }

/// Get an entry in the scope. This allows the value stored in the scope to be
/// modified if needed.
let public entry scope id =
    List.tryFind (fun e -> e.Id = id) scope.Entries

/// Find an entry in the scope.
let public find scope id =
    entry scope id |> Option.map (fun x -> x.Value)

/// Add an entry to the given scope. Returns a new scope with the given entry
/// added at the current depth.
let public insert scope id value =
    let entry = { Value = value; Id = id; Depth = scope.CurrentDepth }
    { scope with Entries = entry::scope.Entries; }

/// Create a scope from the initial environment map
let public fromMap map =
    Map.fold insert empty map