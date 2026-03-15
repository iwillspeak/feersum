namespace Feersum.CompilerServices.Syntax

open Firethorn
open Feersum.CompilerServices.Text

/// A position in the source text. This is used for error reporting and other
/// diagnostics. This is a simple wrapper around a `TextRange` with a reference to
/// the source document for easier error reporting.
///
/// TODO: Later we _should_ move this to use a nominal `DocID` rather than
/// a direct reference to the `TextDocument`. This will make it easier to support multiple documents in the future, and will also make it easier to
/// serialize and deserialize syntax trees for pickling into assemblies.
type StxPos = { Doc: TextDocument; Range: TextRange }

/// Syntax Identifer
///
/// Syntax identifiers represent a specific occurence of a name within the
/// source text. Identifiers are a named pair where each distinct syntactic
/// occurence of a given name has a uniqe `Stamp` attached to tell them apart.
type Ident = { Name: string; Stamp: int }

module Ident =
    open System.Threading

    [<Literal>]
    let public GlobalStamp = 0

    let private nextStamp = ref 0

    /// Mint a new `Ident` with the given name and the default `Stamp` of 0.
    /// Used by the parser to create `Ident`s for names in the source text.
    let mint (name: string) : Ident = { Name = name; Stamp = GlobalStamp }

    /// Re-Stamp an `ident` with a fresh `Stamp` value.
    let fresh (ident: Ident) : Ident =
        { ident with
            Stamp = Interlocked.Increment nextStamp }

/// Special Forms. These represent the unique forms we have to handle either
/// in syntax expansion or binding.
type SpecialFormKind =
    | If
    | Define
    | DefineSyntax
    | Lambda
    | Quote
    | Let
    | LetStar
    | LetRec
    | LetRecStar

/// Binding and Expansion Tree
///
/// The `Stx` tree modells the syntax of the source text in a slightly more
/// abstract way than the CST `Tree`. This is used as the input and output
/// format for **Syntax Expansion**. The `Stx` tree has a few additioanl parts
/// to accomplish this:
///
///  * Each name in the tree is resolved to an `Ident` rather than a raw string.
///  * The `StxClosure` variant allows syntax captures from macro expansion.
type Stx =
    | Symbol of Ident * StxPos
    | Literal of StxLiteral * StxPos
    | Form of Stx list * Stx option * StxPos
    | Closure of Stx * StxEnv

/// A source literal is eitehr a self-evaluating constant, or a quoted datum.
and StxLiteral =
    | Quotation of StxDatum * StxPos
    | SelfEval of StxConstant * StxPos

/// A datume is a literal value baked into source text. This could be as simple
/// as an identifer or a quoted constant, or as complex as nested lists or
/// vectors.
and StxDatum =
    | Simple of StxConstant * StxPos
    | Ident of Ident * StxPos
    | List of StxDatum list * StxDatum option * StxPos
    | Compound of Stx

/// A constant is a literal value that evaluates to itself.
and StxConstant =
    | Boolean of bool
    | Character of char
    | Number of double
    | Str of string
    | Vector of StxDatum list
    | ByteVec of byte list
    | Null

/// Environment for syntax expansion. Maps `Ident`s to `StxBinding`s.
and StxEnv = Map<Ident, StxBinding>

/// Bound Value in the `StxEnv`. Identifiers are either references to
/// variables or syntax items. Syntax can be either a well known special form
/// or a macro transformer.
and StxBinding =
    | Var of Ident
    | Macro of Transfomer
    | SpecialForm of SpecialFormKind

/// A tranformer is a function that takes an `Stx` and an `StxEnv` and produces
/// a new `Stx`.
and Transfomer = Stx -> StxEnv -> Stx

/// Syntax Context Utils
module Stx =

    /// Get the `TextRange` of a `Stx`. This is used for error reporting and
    /// other diagnostics.
    let rec getPos (stx: Stx) : StxPos =
        match stx with
        | Symbol(_, pos) -> pos
        | Form(_, _, pos) -> pos
        | Closure(stx, _) -> getPos stx
        | Literal(_, pos) -> pos

/// Syntax Environment Helpers
module StxEnv =

    /// Close a `Stx` with an `StxEnv` to produce a `Stx.Closure`
    let close (stx: Stx) (env: StxEnv) : Stx = Closure(stx, env)

    /// Resolve an `id` in the given `env`. Unbound values will resolve to
    /// their "default" binding.
    let resolve (id: Ident) (env: StxEnv) : StxBinding =
        match env.TryFind id with
        | Some binding -> binding
        | None ->
            // If the identifier isn't bound, it takes on the 'default' meaning.
            // For keywords this is their marker symbol. For other identifiers this
            // is a variable in the scope 0 context (global).
            match id.Name with
            | "lambda" -> SpecialForm Lambda
            | "let" -> SpecialForm Let
            | "let*" -> SpecialForm LetStar
            | "letrec" -> SpecialForm LetRec
            | "letrec*" -> SpecialForm LetRecStar
            | "if" -> SpecialForm If
            | "define" -> SpecialForm Define
            | "define-syntax" -> SpecialForm DefineSyntax
            | "quote" -> SpecialForm Quote
            | _ -> Var id

    /// Rename an `Ident` in the given `env` to a fresh `Ident`.
    ///
    /// Returns the new `Ident` and an updated `StxEnv` with the new binding.
    let rename (id: Ident) (env: StxEnv) : Ident * StxEnv =
        let newId = Ident.fresh id
        newId, env.Add(id, Var newId)
