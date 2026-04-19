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
type StorageRef =
    | Local of int
    | Global of string * GlobalType
    | Arg of int
    | Environment of int * StorageRef
    | Captured of StorageRef

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
        | Some(StrVal(Result.Ok s)) -> BoundLiteral.Str s
        | Some(StrVal(Result.Error _)) -> BoundLiteral.Null
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
