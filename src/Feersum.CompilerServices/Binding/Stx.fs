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
    | Boolean  of bool
    | Number   of double
    | Character of char
    | Str      of string
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

    member x.Loc =
        match x with
        | Id(_, l)
        | Datum(_, l)
        | List(_, _, l)
        | Closure(_, _, l)
        | Vec(_, l) -> l

/// A Single Binding in the Syntax Environment
/// 
/// Syntax names can be bound to one of three things:
/// 1. A special form, which is a built-in syntax transformer that is handled
///    directly by the expander. These are things like `if`, `lambda`, `define`,
///    etc. which have special meaning during binding.
/// 2. A macro, which is a user-defined syntax transformer. These are defined by
///    `define-syntax`, `let-syntax`, etc.
/// 3. A variable, which is a binding of a name to an identifier. These are
///    introduced by `define`, `let`, and friends. and allow us to tell multiple
///    identical names from different syntactic contexts apart for hygiene.
and StxBinding =
    | Special of SpecialFormKind
    | Macro of SyntaxTransformer
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

    /// Convert a raw CST `Expression` into an `Stx` object ready for expansion.
    let rec ofExpr (reg: SourceRegistry) (docId: DocId) (expr: Expression) : Stx =
        let loc = SourceRegistry.resolveLocation reg docId expr.SyntaxRange

        match expr with
        | SymbolNode s -> Stx.Id(s.CookedValue, loc)
        | ConstantNode c ->
            let datum =
                match c.Value with
                | Some(NumVal n)  -> StxDatum.Number n
                | Some(StrVal s)  -> StxDatum.Str s
                | Some(BoolVal b) -> StxDatum.Boolean b
                | Some(CharVal c) -> StxDatum.Character(Option.defaultValue '\u0000' c)
                | None            -> StxDatum.Boolean false  // error recovery

            Stx.Datum(datum, loc)
        | FormNode f ->
            let body = f.Body |> Seq.map (ofExpr reg docId) |> List.ofSeq

            match f.DottedTail with
            | None -> Stx.List(body, None, loc)
            | Some tail ->
                let t =
                    tail.Body
                    |> Option.map (ofExpr reg docId)
                    |> Option.defaultValue (Stx.List([], None, loc))

                Stx.List(body, Some t, loc)
        | QuotedNode q ->
            // `'datum` is reader sugar for `(quote datum)` — desugar here at
            // the CST boundary so Stx never carries a dedicated Quoted node.
            let inner =
                q.Inner
                |> Option.map (ofExpr reg docId)
                |> Option.defaultValue (Stx.List([], None, loc))

            Stx.List([ Stx.Id("quote", loc); inner ], None, loc)
        | VecNode v ->
            let items = v.Body |> Seq.map (ofExpr reg docId) |> List.ofSeq
            Stx.Vec(items, loc)
        | ByteVecNode b ->
            let bytes =
                b.Body |> List.choose (fun bv -> match bv.Value with Ok b -> Some b | Result.Error _ -> None)

            Stx.Datum(StxDatum.ByteVector bytes, loc)
