namespace Feersum.CompilerServices.BindingNew

open Feersum.CompilerServices.Syntax

/// Well known syntax forms
type SpecialFormKind =
    | If
    | Let
    | Quote
    | Lambda
    | Define
    | Set

/// Syntactic Identifier
///
/// An identifier is a single stamped name. We stamp both a unique stamp for the
/// binding site as well as the
[<Struct>]
type private Identifier =
    { Name: string
      Stamp: int
      Scope: ScopeId }

/// Syntax Binding
///
/// In syntax there are two main classes of item: Syntactic and variables.
/// Syntacic items can be either well known builtins, or macros.
and private SyntaxBinding =
    | SpecialForm of SpecialFormKind
    | Macro
    | Variable of Identifier

/// Scope Identifier
and ScopeId = ScopeId of int

/// Expansion Environment
///
/// Used to hold the currently active syntax bindings and environment bindings
/// during binding. We need two separate maps here to distinguish between syntax
/// whcih controls which names resolve to which syntactic identifiers, and the
/// exeuction environment which holds the binding of idnentifiers to their
/// storage location.
type private ExpandEnv =
    { SyntaxEnv: Map<string, SyntaxBinding>
      Env: Map<string, Binding>
      Parent: ExpandEnv option }

/// Global binding Type
///
/// For code we generate globals are stored in static fields. For some imported
/// references globals may be stored in static fields instead.
and GlobalType =
    | Method of string
    | Field of string

/// Value Binding
///
/// Reprsents a bound storage location for an identifier. This can be a local
///  variable, a global variable, an argument, or captured item.
and Binding =
    | Local of int
    | Global of string * GlobalType
    | Arg of int
    | Environment of int * Binding
    | Captured of Binding


module private Impl =

    let expandWithEnv (env: ExpandEnv) (prog: Tree.Program) =
        // For now, this is a no-op. In the future, this will perform macro expansion and
        // other transformations on the syntax tree before we start binding.
        prog

/// Public API for the new binding & expansion.
module Expand =

    /// Expand a single program into a bound tree
    let expandProgram (prog: Tree.Program) =
        let ctx =
            { SyntaxEnv = Map.empty
              Env = Map.empty
              Parent = None }

        Impl.expandWithEnv ctx prog
