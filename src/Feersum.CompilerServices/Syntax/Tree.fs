namespace Feersum.CompilerServices.Syntax.Tree

open Firethorn
open Firethorn.Green
open Firethorn.Red
open Feersum.CompilerServices.Text

/// Node kind for each element in the raw tree.
type AstKind =
    | ERROR = -1

    // nodes
    | SCRIPT_PROGRAM = 1
    | PROGRAM = 2
    | CONSTANT = 3
    | SYMBOL = 4
    | FORM = 5
    | QUOTED_DATUM = 6
    | VEC = 7
    | BYTEVEC = 8
    | DOTTED_TAIL = 9

    // tokens
    | EOF = 101
    | NUMBER = 102
    | STRING = 103
    | BOOLEAN = 104
    | CHARACTER = 105
    | IDENTIFIER = 106
    | ATMOSPHERE = 107
    | OPEN_PAREN = 108
    | CLOSE_PAREN = 109
    | QUOTE = 110
    | DOT = 111

module SyntaxUtils =

    /// Convert an AST Kind to a raw `SyntaxKind`
    let astToGreen (kind: AstKind) = SyntaxKind(int kind)

    /// Convert a raw kind back to an `AstKind`
    let greenToAst =
        function
        | SyntaxKind kind -> enum<AstKind> kind

    /// Debug dump. Writes a debug representation of the tree to stdout.
    let dump = Debug.debugDump (Debug.mappedFormatter greenToAst)

    /// Pretty print the syntax tree to a string, with mapped node names
    let prettyPrint = Debug.debugToString (Debug.mappedFormatter greenToAst)

open SyntaxUtils
open System.Text

[<AutoOpen>]
module private Utils =

    /// Predicate funtion to filter tokesn by AST Kind
    let tokenOfKind (kind: AstKind) (token: SyntaxToken) = token.Kind = (kind |> astToGreen)

    type CookingState =
        | Plain
        | InEscape
        | InHex of string

    /// Cook a string value, replacing escapes with values. Returns an error
    /// message if the string contains a malformed or out-of-range hex escape.
    let cookString (s: string) : Result<string, string> =

        let cookChar ((state: CookingState), (sb: StringBuilder), (err: string option)) (ch: char) =
            match state with
            | Plain ->
                match ch with
                | '\\' -> (InEscape, sb, err)
                | _ -> (Plain, sb.Append(ch), err)
            | InHex buff ->
                match ch with
                | ';' ->
                    if buff = "0x" then
                        (Plain, sb, err |> Option.orElse (Some "malformed hex escape"))
                    else
                        match System.Int64.TryParse(buff[2..], System.Globalization.NumberStyles.HexNumber, null) with
                        | true, v when v >= 0L && v <= int64 System.Char.MaxValue ->
                            (Plain, sb.Append(char (int v)), err)
                        | _ -> (Plain, sb, err |> Option.orElse (Some "hex escape value out of range"))
                | '0'
                | '1'
                | '2'
                | '3'
                | '4'
                | '5'
                | '6'
                | '7'
                | '8'
                | '9'
                | 'a'
                | 'b'
                | 'c'
                | 'd'
                | 'e'
                | 'f'
                | 'A'
                | 'B'
                | 'C'
                | 'D'
                | 'E'
                | 'F' -> (InHex(buff + (string ch)), sb, err)
                | _ -> (Plain, sb, err |> Option.orElse (Some "malformed hex escape"))
            | InEscape ->
                match ch with
                | 'a' -> (Plain, sb.Append('\a'), err)
                | 'b' -> (Plain, sb.Append('\b'), err)
                | 't' -> (Plain, sb.Append('\t'), err)
                | 'n' -> (Plain, sb.Append('\n'), err)
                | 'v' -> (Plain, sb.Append('\v'), err)
                | 'f' -> (Plain, sb.Append('\f'), err)
                | 'r' -> (Plain, sb.Append('\r'), err)
                | 'x' -> (InHex "0x", sb, err)
                | _ -> (Plain, sb.Append(ch), err)

        let finalState, sb, firstErr =
            s |> Seq.fold cookChar (CookingState.Plain, StringBuilder(), None)

        let err =
            match finalState, firstErr with
            | InHex _, None -> Some "unterminated hex escape"
            | _ -> firstErr

        match err with
        | Some msg -> Error msg
        | None -> Ok(sb.ToString())


    /// Cook the value of an identifier. This takes the raw identifier and
    /// converts it into a 'cooked' form, expanding out escaped values and
    /// replacing them with the true characters.
    let cookIdentifier (token: SyntaxToken) : Result<string, string> =
        let tokenText = token.Green.Text

        if tokenText.StartsWith('|') && tokenText.EndsWith('|') then
            tokenText[1 .. (tokenText.Length - 2)] |> cookString
        else
            Ok tokenText


/// ------------------------ Syntax Tree Types --------------------------------
///
/// The syntax tree is made up of three laysers. The red and green layers come
/// from the Firethorn library. This last layer is a colleciton of classes that
/// 'query' the underlying untyped tree to provide structured access to the
/// data.
///
/// Root type in the AST Tree. All node types should inherit from this
/// either directly or indeirectly .

[<AbstractClass>]
type AstItem internal (red: NodeOrToken<SyntaxNode, SyntaxToken>) =

    /// Get the Syntax range of the item
    member public _.SyntaxRange = red |> NodeOrToken.consolidate (_.Range) (_.Range)

    member _.Text =
        red
        |> NodeOrToken.consolidate
            (fun n ->
                Walk.walk n
                |> Seq.fold
                    (fun (sb: StringBuilder) ev ->
                        match ev with
                        | OnToken t -> sb.Append(t.Green.Text)
                        | _ -> sb)
                    (new StringBuilder())
                |> _.ToString())
            _.Green.Text

    override _.ToString() =
        red |> NodeOrToken.consolidate (_.ToString()) (_.ToString())

// *********** TOKENS

[<AbstractClass>]
type AstToken internal (red: SyntaxToken) =

    inherit AstItem(red |> Token)

    member public _.RawToken = red

type ByteValue internal (red: SyntaxToken) =

    inherit AstToken(red)

    static member TryCast(red: SyntaxToken) =
        match red.Kind |> greenToAst with
        | AstKind.NUMBER -> Some(new ByteValue(red))
        | _ -> None

    member public x.Value =
        match red.Green.Text |> System.Byte.TryParse with
        | true, value -> Ok value
        | _ -> Error(sprintf "Invalid byte value: %s" red.Green.Text)

[<AbstractClass>]
type ConstantValue internal (red: SyntaxToken) =

    inherit AstToken(red)

    static member TryCast(red: SyntaxToken) =
        match red.Kind |> greenToAst with
        | AstKind.STRING -> Some(new StrVal(red) :> ConstantValue)
        | AstKind.NUMBER -> Some(new NumVal(red))
        | AstKind.BOOLEAN -> Some(new BoolVal(red))
        | AstKind.CHARACTER -> Some(new CharVal(red))
        | _ -> None

/// Number node in the syntax tree.
and NumVal internal (red: SyntaxToken) =

    inherit ConstantValue(red)

    member public x.Value =
        match red.Green.Text |> System.Double.TryParse with
        | (true, value) -> value
        | _ -> 0.0

/// String node in the syntax tree.
and StrVal internal (red: SyntaxToken) =

    inherit ConstantValue(red)

    member public x.Value =
        let text = red.Green.Text
        text[1 .. text.Length - 2] |> cookString

/// Boolean node in the syntax tree.
and BoolVal internal (red: SyntaxToken) =

    inherit ConstantValue(red)

    member public x.Value = x.Text.StartsWith("#t")


/// Character node in the syntax tree.
and CharVal internal (red: SyntaxToken) =

    inherit ConstantValue(red)

    member public _.Value =
        let charText = red.Green.Text

        if charText.Length = 3 then
            Some(charText[2])
        else if charText.StartsWith("#\\x") then
            match System.Int32.TryParse(charText[3..], System.Globalization.NumberStyles.HexNumber, null) with
            | true, hex -> Some((char) hex)
            | _ -> None
        else
            match charText[2..] with
            | "alarm" -> Some('\u0007')
            | "backspace" -> Some('\u0008')
            | "delete" -> Some('\u007F')
            | "escape" -> Some('\u001B')
            | "newline" -> Some('\u000A')
            | "null" -> Some('\u0000')
            | "return" -> Some('\u000D')
            | "space" -> Some(' ')
            | "tab" -> Some('\u0009')
            | _ -> None


// *********** NODES

[<AbstractClass>]
type AstNode internal (red: SyntaxNode) =

    inherit AstItem(red |> Node)

    member public _.RawNode = red

/// Form syntax wrapper
type Form internal (red: SyntaxNode) =

    inherit Expression(red)

    member public _.OpeningParen =
        red.ChildrenWithTokens()
        |> Seq.choose (NodeOrToken.asToken)
        |> Seq.tryFind (tokenOfKind AstKind.OPEN_PAREN)

    member public _.Body = red.Children() |> Seq.choose (fun n -> Expression.TryCast(n))

    member public _.DottedTail =
        red.Children() |> Seq.tryPick (fun n -> DottedTail.TryCast(n))

    member public _.ClosingParen =
        red.ChildrenWithTokens()
        |> Seq.choose (NodeOrToken.asToken)
        |> Seq.tryFind (tokenOfKind AstKind.CLOSE_PAREN)

and DottedTail internal (red: SyntaxNode) =

    inherit AstNode(red)

    member public _.Body = red.Children() |> Seq.tryPick (fun n -> Expression.TryCast(n))

    static member TryCast(node: SyntaxNode) =
        match node.Kind |> greenToAst with
        | AstKind.DOTTED_TAIL -> DottedTail(node) |> Some
        | _ -> None

/// Symbolic identifier node. This wraps an indentifier token when it is used
/// as a symbol in the source text.
and Symbol internal (red: SyntaxNode) =

    inherit Expression(red)

    member public _.CookedValue =
        red.ChildrenWithTokens()
        |> Seq.choose (NodeOrToken.asToken)
        |> Seq.tryExactlyOne
        |> Option.map (cookIdentifier)
        |> Option.defaultValue (Ok "")

/// This is a self-evaluating expression that contains a sngle constant datum.
and Constant internal (red: SyntaxNode) =

    inherit Expression(red)

    member public _.Value =
        red.ChildrenWithTokens()
        |> Seq.choose (NodeOrToken.asToken)
        |> Seq.tryExactlyOne
        |> Option.bind (fun t -> ConstantValue.TryCast(t))

/// Quoted datum node
and Quoted internal (red: SyntaxNode) =

    inherit Expression(red)

    member x.Inner = x.RawNode.Children() |> Seq.tryPick (fun n -> Expression.TryCast(n))

/// Vector constant node
and Vec internal (red: SyntaxNode) =

    inherit Expression(red)

    member x.Body = x.RawNode.Children() |> Seq.choose (fun n -> Expression.TryCast(n))

/// Byte Vector constant node
and ByteVec internal (red: SyntaxNode) =

    inherit Expression(red)

    member x.Body =
        x.RawNode.ChildrenWithTokens()
        |> Seq.choose (NodeOrToken.asToken)
        |> Seq.choose (fun t -> ByteValue.TryCast(t))
        |> Seq.toList

/// Expression value. This is the set of all expression types. Expressions can
/// be either a simple datum (`Constant`), an identifier `Symbol`, or a comple
/// `Form` datum.
and Expression internal (red: SyntaxNode) =
    inherit AstNode(red)

    static member TryCast(node: SyntaxNode) =
        match node.Kind |> greenToAst with
        | AstKind.FORM -> Some(new Form(node) :> Expression)
        | AstKind.VEC -> Some(new Vec(node))
        | AstKind.BYTEVEC -> Some(new ByteVec(node))
        | AstKind.SYMBOL -> Some(new Symbol(node))
        | AstKind.CONSTANT -> Some(new Constant(node))
        | AstKind.QUOTED_DATUM -> Some(new Quoted(node))
        | _ -> None

/// Root AST type for script expressions.
type ScriptProgram internal (red: SyntaxNode) =

    inherit AstNode(red)

    member _.Body =
        red.Children()
        |> Seq.choose (fun n -> Expression.TryCast(n))
        |> Seq.tryExactlyOne

/// Wrapper type to represent an entire parsed program. This represents the
/// contents of a single compilation unit.
type Program internal (red: SyntaxNode) =

    inherit AstNode(red)

    member _.Body = red.Children() |> Seq.choose (fun n -> Expression.TryCast(n))

/// Active patterns to make working with elements in the syntax tree more
/// ergonomic.
[<AutoOpen>]
module Patterns =

    open Feersum.CompilerServices.Ice

    /// Pattern to match on known expression types
    let (|ByteVecNode|VecNode|FormNode|ConstantNode|SymbolNode|QuotedNode|) (expr: Expression) =
        match expr with
        | :? ByteVec as b -> ByteVecNode b
        | :? Vec as v -> VecNode v
        | :? Form as f -> FormNode f
        | :? Constant as c -> ConstantNode c
        | :? Symbol as s -> SymbolNode s
        | :? Quoted as q -> QuotedNode q
        | _ -> icef "Unexpected expression type: %A" (expr.GetType())

    /// Ergonomic pattern to match the useful inner parts of an expression
    let (|ByteVec|Vec|Form|DottedForm|Constant|Symbol|Quoted|) (expr: Expression) =
        match expr with
        | ByteVecNode b -> ByteVec
        | VecNode v -> Vec
        | FormNode f ->
            let mainBody = (f.Body |> List.ofSeq)

            match f.DottedTail with
            | None -> Form mainBody
            | Some tail -> DottedForm(mainBody, tail.Body)
        | ConstantNode c -> Constant c.Value
        | SymbolNode s -> Symbol s.CookedValue
        | QuotedNode q -> Quoted q.Inner

    /// Pattern to match on known constant types
    let (|NumValNode|StrValNode|BoolValNode|CharValNode|) (cnst: ConstantValue) =
        match cnst with
        | :? NumVal as n -> NumValNode n
        | :? StrVal as s -> StrValNode s
        | :? BoolVal as b -> BoolValNode b
        | :? CharVal as c -> CharValNode c
        | _ -> icef "Unexpected constant type: %A" (cnst.GetType())

    /// Ergonomic pattern to match the inner parts of a constant value
    let (|NumVal|StrVal|BoolVal|CharVal|) (cnst: ConstantValue) =
        match cnst with
        | NumValNode n -> NumVal n.Value
        | StrValNode s -> StrVal s.Value
        | BoolValNode b -> BoolVal b.Value
        | CharValNode c -> CharVal c.Value
