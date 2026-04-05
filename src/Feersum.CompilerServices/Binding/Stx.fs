namespace Feersum.CompilerServices.Binding

open Feersum.CompilerServices.Text
open Feersum.CompilerServices.Diagnostics

/// Well-known built-in transformer kinds.
[<RequireQualifiedAccess>]
type SpecialFormKind =
    | If
    | Lambda
    | Define
    | SetBang
    | Begin
    | Quote
    | Let
    | LetStar
    | Letrec
    | LetrecStar
    | DefineSyntax
    | LetSyntax
    | LetrecSyntax
    | Import
    | DefineLibrary

/// A globally unique identity for a single binding introduction.
/// Variables with the same name but introduced at different points get
/// different BindingIds, enabling hygienic expansion without gensyms.
[<Struct>]
type Ident = private Ident of int

module Ident =
    open System.Threading

    let mutable private counter = 0

    let fresh () =
        let id = Interlocked.Increment(&counter)
        Ident id


/// A self-evaluating atomic datum at the syntactic level.
/// These are the R7RS simple datums: booleans, numbers, characters, strings,
/// and bytevectors.  Vectors are compound (Stx.Vec); the empty list / null is
/// Stx.List([], None, loc).
[<RequireQualifiedAccess>]
type StxDatum =
    | Boolean of bool
    | Number of double
    | Character of char
    | Str of string
    | ByteVector of byte list


/// The intermediate hygiene-annotated syntax type.
/// Every node carries a source location; `Closure` nodes carry the definition-site scope.
[<RequireQualifiedAccess>]
type Stx =
    /// An identifier (raw, unresolved name).
    | Id of name: string * loc: TextLocation
    /// A self-evaluating datum — number, boolean, character, string, or bytevector.
    | Datum of value: StxDatum * loc: TextLocation
    /// A list — `(e1 e2 … en)` when tail is None; `(e1 … en . t)` when tail is Some t.
    | List of items: Stx list * tail: Stx option * loc: TextLocation
    /// A syntactic closure — expand `inner` in `env` rather than the ambient scope.
    | Closure of inner: Stx * env: StxEnvironment * loc: TextLocation
    /// A vector literal — `#(e1 e2 … en)`.
    | Vec of items: Stx list * loc: TextLocation
    /// A reader-level error node — produced by `Stx.ofExpr` when the CST
    /// contains malformed syntax (invalid byte, missing char value, etc.).
    /// The expander treats this as a silent no-op error so downstream passes
    /// don't emit duplicate diagnostics.
    | Error of loc: TextLocation

    member x.Loc =
        match x with
        | Id(_, l)
        | Datum(_, l)
        | List(_, _, l)
        | Closure(_, _, l)
        | Vec(_, l)
        | Error l -> l

/// A Single Binding in the Syntax Environment
///
/// Syntax names can be bound to one of three things:
/// 1. A special form, which is a built-in syntax transformer that is handled
///    directly by the expander. These are things like `if`, `lambda`, `define`,
///    etc. which have special meaning during binding.
/// 2. A macro, which is a user-defined syntax transformer. These are defined by
///    `define-syntax`, `let-syntax`, etc. The transformer implementation is
///    stored separately in `MacroRegistry` and looked up by this Ident at
///    expansion time. This indirection enables `letrec-syntax` scoping: all
///    macro Idents can be placed into the definition-site scope before any
///    transformer is parsed, so each transformer sees the full mutually-recursive
///    scope.
/// 3. A variable, which is a binding of a name to an identifier. These are
///    introduced by `define`, `let`, and friends. and allow us to tell multiple
///    identical names from different syntactic contexts apart for hygiene.
and StxBinding =
    | Special of SpecialFormKind
    | Macro of Ident
    | Variable of Ident

/// The syntax environment maps syntax names to their bindings.
and StxEnvironment = Map<string, StxBinding>

/// A syntax transformer is a function that takes the full macro-call form (as a
/// single <c>Stx</c> node), the call-site syntax environment, and produces either
/// a new syntax object (the result of the macro expansion) or an error message if
/// there was an error during expansion.
and SyntaxTransformer = Stx -> StxEnvironment -> Result<Stx, string>

module Stx =
    open Feersum.CompilerServices.Syntax.Tree
    open Feersum.CompilerServices.Ice

    // ── Closure peeling ───────────────────────────────────────────────────

    /// Strip all `Closure` wrappers from `stx`, returning the innermost shape
    /// and the innermost explicit environment (if any closure was present).
    ///
    /// - `Some env` — at least one `Closure` was present; `env` is authoritative
    ///   for expanding the returned node's subforms.
    /// - `None`     — no `Closure` wrapper; the caller should use the ambient scope.
    let peel (stx: Stx) : Stx * StxEnvironment option =
        let rec go envOpt s =
            match s with
            | Stx.Closure(inner, env, _) -> go (Some env) inner
            | other -> other, envOpt

        go None stx

    /// Active pattern that peels any `Closure` wrappers and then mirrors every
    /// non-`Closure` case of the `Stx` DU.
    ///
    /// The resolved `StxEnvironment option` is surfaced on cases whose subforms
    /// need to be expanded in scope:
    /// - `None`     — node arrived bare; expand subforms in the ambient scope.
    /// - `Some env` — node was closure-wrapped; `env` is authoritative.
    ///
    /// `Datum`, `Vec`, and `Error` omit the environment because they carry no
    /// subforms that require scope resolution.
    let (|Id|Datum|List|Vec|Error|) (stx: Stx) =
        let shape, envOpt = peel stx

        match shape with
        | Stx.Id(name, loc) -> Id(name, loc, envOpt)
        | Stx.Datum(value, loc) -> Datum(value, loc)
        | Stx.List(items, tail, loc) -> List(items, tail, loc, envOpt)
        | Stx.Vec(items, loc) -> Vec(items, loc)
        | Stx.Error loc -> Error loc
        | Stx.Closure _ -> ice "peel returned a Closure (impossible)"

    // ── CST conversion ─────────────────────────────────────────────────────

    let private malformedDatum =
        DiagnosticKind.Create DiagnosticLevel.Error 57 "Invalid datum value"

    /// Convert a raw CST `Expression` into an `Stx` object ready for expansion.
    /// Emits diagnostics into `diag` for reader-level errors such as malformed
    /// byte values, missing character values, and empty dotted-pair tails.
    let rec ofExpr (reg: SourceRegistry) (docId: DocId) (diag: DiagnosticBag) (expr: Expression) : Stx =
        let loc = SourceRegistry.resolveLocation reg docId expr.SyntaxRange
        let recurse = ofExpr reg docId diag

        match expr with
        | SymbolNode s -> Stx.Id(s.CookedValue, loc)
        | ConstantNode c ->
            match c.Value with
            | Some(NumVal n) -> Stx.Datum(StxDatum.Number n, loc)
            | Some(StrVal s) -> Stx.Datum(StxDatum.Str s, loc)
            | Some(BoolVal b) -> Stx.Datum(StxDatum.Boolean b, loc)
            | Some(CharVal(Some ch)) -> Stx.Datum(StxDatum.Character ch, loc)
            | Some(CharVal None) ->
                diag.Emit malformedDatum loc "invalid character literal"
                Stx.Error loc
            | None ->
                diag.Emit malformedDatum loc "invalid datum value"
                Stx.Error loc
        | FormNode f ->
            let body = f.Body |> Seq.map recurse |> List.ofSeq

            match f.DottedTail with
            | None -> Stx.List(body, None, loc)
            | Some tail ->
                match tail.Body with
                | Some t -> Stx.List(body, Some(recurse t), loc)
                | None ->
                    diag.Emit malformedDatum loc "dotted pair must have a tail expression"
                    Stx.List(body, None, loc)
        | QuotedNode q ->
            // `'datum` is reader sugar for `(quote datum)` — desugar here at
            // the CST boundary so Stx never carries a dedicated Quoted node.
            match q.Inner with
            | None ->
                diag.Emit malformedDatum loc "empty quotation"
                Stx.Error loc
            | Some inner -> Stx.List([ Stx.Id("quote", loc); recurse inner ], None, loc)
        | VecNode v ->
            let items = v.Body |> Seq.map recurse |> List.ofSeq
            Stx.Vec(items, loc)
        | ByteVecNode b ->
            let results = b.Body |> List.map (fun bv -> bv.Value)

            let errors =
                results
                |> List.choose (function
                    | Result.Error e -> Some e
                    | Ok _ -> None)

            if not (List.isEmpty errors) then
                errors |> List.iter (fun msg -> diag.Emit malformedDatum loc msg)
                Stx.Error loc
            else
                let bytes =
                    results
                    |> List.choose (function
                        | Ok by -> Some by
                        | _ -> None)

                Stx.Datum(StxDatum.ByteVector bytes, loc)
