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
/// Stx.List([], None, pos).
[<RequireQualifiedAccess>]
type StxDatum =
    | Boolean of bool
    | Number of double
    | Character of char
    | Str of string
    | ByteVector of byte list


/// A compact source position for a syntax node.
/// Stores the document and raw Firethorn range, deferring the expensive
/// offset→line/column conversion until a human-readable location is needed.
[<Struct>]
type StxPos =
    { Doc: TextDocument
      Range: Firethorn.TextRange }

    /// Resolve to a human-readable TextLocation. Performs a binary search
    /// over LineStarts; call only when a diagnostic or sequence point is needed.
    member x.Location = TextDocument.rangeToLocation x.Doc x.Range

module StxPos =
    /// Sentinel value for compiler-synthesised nodes that have no source origin.
    let missing =
        { Doc = { Path = "missing"; LineStarts = [] }
          Range = Unchecked.defaultof<Firethorn.TextRange> }


/// The intermediate hygiene-annotated syntax type.
/// Every node carries a source position; `Closure` nodes carry the definition-site scope.
[<RequireQualifiedAccess>]
type Stx =
    /// An identifier (raw, unresolved name).
    | Id of name: string * pos: StxPos
    /// A self-evaluating datum — number, boolean, character, string, or bytevector.
    | Datum of value: StxDatum * pos: StxPos
    /// A list — `(e1 e2 … en)` when tail is None; `(e1 … en . t)` when tail is Some t.
    | List of items: Stx list * tail: Stx option * pos: StxPos
    /// A syntactic closure — expand `inner` in `env` rather than the ambient scope.
    | Closure of inner: Stx * env: StxEnvironment
    /// A vector literal — `#(e1 e2 … en)`.
    | Vec of items: Stx list * pos: StxPos
    /// A reader-level error node — produced by `Stx.ofExpr` when the CST
    /// contains malformed syntax (invalid byte, missing char value, etc.).
    /// The expander treats this as a silent no-op error so downstream passes
    /// don't emit duplicate diagnostics.
    | Error of pos: StxPos

    member x.Pos =
        match x with
        | Id(_, p)
        | Datum(_, p)
        | List(_, _, p)
        | Vec(_, p)
        | Error p -> p
        | Closure(inner, _) -> inner.Pos


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
            | Stx.Closure(inner, env) -> go (Some env) inner
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
    /// `StxDatum`, `StxVec`, and `StxError` omit the environment because they
    /// carry no subforms that require scope resolution.
    let (|StxId|StxDatum|StxList|StxVec|StxError|) (stx: Stx) =
        let shape, envOpt = peel stx

        match shape with
        | Stx.Id(name, pos) -> StxId(name, pos, envOpt)
        | Stx.Datum(value, pos) -> StxDatum(value, pos)
        | Stx.List(items, tail, pos) -> StxList(items, tail, pos, envOpt)
        | Stx.Vec(items, pos) -> StxVec(items, pos, envOpt)
        | Stx.Error pos -> StxError pos
        | Stx.Closure _ -> ice "peel returned a Closure (impossible)"

    // ── CST conversion ─────────────────────────────────────────────────────

    let private malformedDatum =
        DiagnosticKind.Create DiagnosticLevel.Error 57 "Invalid datum value"

    /// Convert a raw CST `Expression` into an `Stx` object ready for expansion.
    /// Emits diagnostics into `diag` for reader-level errors such as malformed
    /// byte values, missing character values, and empty dotted-pair tails.
    let rec ofExpr (doc: TextDocument) (diag: DiagnosticBag) (expr: Expression) : Stx =
        let pos = { Doc = doc; Range = expr.SyntaxRange }
        let recurse = ofExpr doc diag

        match expr with
        | SymbolNode s ->
            match s.CookedValue with
            | Ok cooked -> Stx.Id(cooked, pos)
            | Error msg ->
                diag.Emit malformedDatum pos.Location msg
                Stx.Error pos
        | ConstantNode c ->
            match c.Value with
            | Some(NumVal n) -> Stx.Datum(StxDatum.Number n, pos)
            | Some(StrVal(Ok s)) -> Stx.Datum(StxDatum.Str s, pos)
            | Some(StrVal(Error msg)) ->
                diag.Emit malformedDatum pos.Location msg
                Stx.Error pos
            | Some(BoolVal b) -> Stx.Datum(StxDatum.Boolean b, pos)
            | Some(CharVal(Some ch)) -> Stx.Datum(StxDatum.Character ch, pos)
            | Some(CharVal None) ->
                diag.Emit malformedDatum pos.Location "invalid character literal"
                Stx.Error pos
            | None ->
                diag.Emit malformedDatum pos.Location "invalid datum value"
                Stx.Error pos
        | FormNode f ->
            let body = f.Body |> Seq.map recurse |> List.ofSeq

            match f.DottedTail with
            | None -> Stx.List(body, None, pos)
            | Some tail ->
                match tail.Body with
                | Some t -> Stx.List(body, Some(recurse t), pos)
                | None ->
                    diag.Emit malformedDatum pos.Location "dotted pair must have a tail expression"
                    Stx.List(body, None, pos)
        | QuotedNode q ->
            // `'datum` is reader sugar for `(quote datum)` — desugar here at
            // the CST boundary so Stx never carries a dedicated Quoted node.
            match q.Inner with
            | None ->
                diag.Emit malformedDatum pos.Location "empty quotation"
                Stx.Error pos
            | Some inner -> Stx.List([ Stx.Id("quote", pos); recurse inner ], None, pos)
        | VecNode v ->
            let items = v.Body |> Seq.map recurse |> List.ofSeq
            Stx.Vec(items, pos)
        | ByteVecNode b ->
            let results = b.Body |> List.map (fun bv -> bv.Value)

            let errors =
                results
                |> List.choose (function
                    | Result.Error e -> Some e
                    | Ok _ -> None)

            if not (List.isEmpty errors) then
                errors |> List.iter (fun msg -> diag.Emit malformedDatum pos.Location msg)
                Stx.Error pos
            else
                let bytes =
                    results
                    |> List.choose (function
                        | Ok by -> Some by
                        | _ -> None)

                Stx.Datum(StxDatum.ByteVector bytes, pos)

    /// Map a well-known keyword name to its `SpecialFormKind`, or return `None`
    /// if the name is not a built-in special form.
    let private tryResolveSpecial (name: string) : SpecialFormKind option =
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

    /// Resolve the binding of `name` in `env`, returning either the binding
    /// or `None` if no binding was found.
    let resolve (name: string) (env: StxEnvironment) : StxBinding option =
        env
        |> Map.tryFind name
        |> Option.orElseWith (fun () -> tryResolveSpecial name |> Option.map StxBinding.Special)

    /// Add a new macro binding for `name` to `stxEnv`
    ///
    /// Defines a fresh `Ident` for the macro and returns it along with the updated environment.
    let reserveMacro (stxEnv: StxEnvironment) (name: string) : Ident * StxEnvironment =
        let id = Ident.fresh ()
        id, Map.add name (StxBinding.Macro id) stxEnv
