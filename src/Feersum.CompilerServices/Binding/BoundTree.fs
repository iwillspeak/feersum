namespace Feersum.CompilerServices.Binding

open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Text
open Feersum.CompilerServices.Syntax.Tree

/// Global binding Type
///
/// For code we generate globals are stored in static fields. For some imported
/// references globals may be stored in static fields instead.
type GlobalType =
    | Method of string
    | Field of string

/// Storage Reference
///
/// Reference to a given storage location. Used to express reads and writes
/// of values to storage locations.
[<CustomComparison; CustomEquality>]
type StorageRef =
    | Macro of Macro
    | Local of int
    | Global of string * GlobalType
    | Arg of int
    | Environment of int * StorageRef
    | Captured of StorageRef

    /// Ordinal for a given StorageRef variant (used in comparison)
    static member private Ordinal =
        function
        | Macro _ -> 0
        | Local _ -> 1
        | Global _ -> 2
        | Arg _ -> 3
        | Environment _ -> 4
        | Captured _ -> 5

    interface System.IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? StorageRef as other ->
                match this, other with
                | Macro m1, Macro m2 ->
                    compare
                        (System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(m1))
                        (System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(m2))
                | Local a, Local b -> compare a b
                | Global(n1, t1), Global(n2, t2) ->
                    let c = compare n1 n2
                    if c <> 0 then c else compare t1 t2
                | Arg a, Arg b -> compare a b
                | Environment(n, s), Environment(m, t) ->
                    let c = compare n m
                    if c <> 0 then c else (s :> System.IComparable).CompareTo(t)
                | Captured s, Captured t -> (s :> System.IComparable).CompareTo(t)
                | _ -> compare (StorageRef.Ordinal this) (StorageRef.Ordinal other)
            | _ -> 0

    override this.Equals(obj) =
        match obj with
        | :? StorageRef as other ->
            match this, other with
            | Macro m1, Macro m2 -> System.Object.ReferenceEquals(m1, m2)
            | Local a, Local b -> a = b
            | Global(n1, t1), Global(n2, t2) -> n1 = n2 && t1 = t2
            | Arg a, Arg b -> a = b
            | Environment(n, s), Environment(m, t) -> n = m && s = t
            | Captured s, Captured t -> s = t
            | _ -> false
        | _ -> false

    override this.GetHashCode() =
        match this with
        | Macro m -> System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(m)
        | Local i -> hash ("Local", i)
        | Global(n, t) -> hash ("Global", n, t)
        | Arg i -> hash ("Arg", i)
        | Environment(n, s) -> hash ("Env", n, s)
        | Captured s -> hash ("Captured", s)

/// Collection of Bound Formal Parameters
///
/// Different types of formal parameters accepted by lambda definitions.
type BoundFormals =
    | Simple of string
    | List of string list
    | DottedList of string list * string

/// Literal or Constant Value
type BoundLiteral =
    | Boolean of bool
    | Character of char
    | Number of double
    | Str of string
    | Vector of BoundDatum list
    | ByteVector of byte list
    | Null

    static member FromConstantValue(cv: ConstantValue option) =
        match cv with
        | Some(NumVal n) -> BoundLiteral.Number n
        | Some(StrVal s) -> BoundLiteral.Str s
        | Some(BoolVal b) -> BoundLiteral.Boolean b
        | Some(CharVal c) -> BoundLiteral.Character(Option.defaultValue '\u0000' c)
        | None -> BoundLiteral.Null

/// Bound Datum Element
///
/// Represents a form or atom when used as a data element rather than as a
/// program element. Used to hold the contents of quoted expressions, as well
/// as the elements within a vector literal.
and BoundDatum =
    | Compound of BoundDatum list
    | Pair of BoundDatum list * BoundDatum
    | SelfEval of BoundLiteral
    | Ident of string
    | Quoted of BoundDatum

/// Bound Expression Type
///
/// Bound expressions represent the syntax of a program with all identifier
/// references resolved to the correct storage.
type BoundExpr =
    | Literal of BoundLiteral
    | Quoted of BoundDatum
    | SequencePoint of BoundExpr * TextLocation
    | Load of StorageRef
    | Store of StorageRef * BoundExpr option
    | Application of BoundExpr * BoundExpr list
    | If of BoundExpr * BoundExpr * BoundExpr option
    | Seq of BoundExpr list
    | Lambda of BoundFormals * BoundBody
    | Library of string list * string * (string * StorageRef) list * BoundBody
    | Import of string
    | Nop
    | Error

and BoundBody =
    { Body: BoundExpr
      Locals: int
      Captures: StorageRef list
      EnvMappings: StorageRef list option }

/// Root type returned by the binder.
type BoundSyntaxTree =
    { Root: BoundBody
      MangledName: string
      Diagnostics: Diagnostic list }
