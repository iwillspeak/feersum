namespace Feersum.CompilerServices.Binding

open Feersum.CompilerServices.Binding
open Feersum.CompilerServices.Binding.Libraries

/// Creates initial binding environments for the expander.
///
/// FIXME: This module contains an odd mix of environment utitlities, syntax environment definitions and binding
///        environments. Really this all needs cleaning up as part of the 'Symbols API' refactor.
module Environments =

    /// Create a preloaded scope from a sequence of library signatures.
    /// Returns a Map<string, StorageRef> containing all exported names from the libraries.
    let fromLibraries (libs: seq<LibrarySignature<StorageRef>>) : Map<string, StorageRef> =
        libs |> Seq.collect (fun lib -> lib.Exports) |> Map.ofSeq

    /// The empty preloaded scope.
    let empty: Map<string, StorageRef> = Map.empty

    /// Map a well-known keyword name to its `SpecialFormKind`, or return `None`
    /// if the name is not a built-in special form.
    let tryResolveSpecial (name: string) : SpecialFormKind option =
        match name with
        | "if" -> Some SpecialFormKind.If
        | "lambda" -> Some SpecialFormKind.Lambda
        | "define" -> Some SpecialFormKind.Define
        | "set!" -> Some SpecialFormKind.SetBang
        | "begin" -> Some SpecialFormKind.Begin
        | "quote" -> Some SpecialFormKind.Quote
        | "let" -> Some SpecialFormKind.Let
        | "let*" -> Some SpecialFormKind.LetStar
        | "letrec" -> Some SpecialFormKind.Letrec
        | "letrec*" -> Some SpecialFormKind.LetrecStar
        | "define-syntax" -> Some SpecialFormKind.DefineSyntax
        | "let-syntax" -> Some SpecialFormKind.LetSyntax
        | "letrec-syntax" -> Some SpecialFormKind.LetrecSyntax
        | "define-library" -> Some SpecialFormKind.DefineLibrary
        | "import" -> Some SpecialFormKind.Import
        | _ -> None

    /// The empty initial scope. Special forms are not stored in the scope;
    /// they are recognised by name in `resolveHead` via `tryResolveSpecial`.
    /// Callers extend this with macro and variable bindings before expansion.
    let builtin: StxEnvironment = Map.empty
