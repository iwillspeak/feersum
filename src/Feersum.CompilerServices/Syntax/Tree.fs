namespace Feersum.CompilerServices.Syntax.Tree

open Firethorn
open Firethorn.Green
open Firethorn.Red

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

module SyntaxUtils =

    /// Convert an AST Kind to a raw `SyntaxKind`
    let astToGreen (kind: AstKind) = SyntaxKind(int kind)

    /// Convert a raw kind back to an `AstKind`
    let greenToAst =
        function
        | SyntaxKind kind -> enum<AstKind> kind

    /// Debug dump. Writes a debug representation of the tree to stdout.
    let dump = Debug.debugDump (Debug.mappedFormatter greenToAst)

open SyntaxUtils

[<AutoOpen>]
module private Utils =

    /// Predicate funtion to filter tokesn by AST Kind
    let tokenOfKind (kind: AstKind) (token: SyntaxToken) = token.Kind = (kind |> astToGreen)

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
    member public _.SyntaxRange =
        red
        |> NodeOrToken.consolidate (fun n -> n.Range) (fun t -> t.Range)

    member _.Text =
        red
        |> NodeOrToken.consolidate (fun n -> n.Green.ToString()) (fun t -> t.Green.Text)

    override _.ToString() =
        red
        |> NodeOrToken.consolidate (fun n -> n.ToString()) (fun t -> t.ToString())


// *********** TOKENS

[<AbstractClass>]
type AstToken internal (red: SyntaxToken) =

    inherit AstItem(red |> Token)

    member public _.RawToken = red

[<AbstractClass>]
type ConstantValue internal (red: SyntaxToken) =

    inherit AstToken(red)

    static member TryCast(red: SyntaxToken) =
        match (red.Kind |> greenToAst) with
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
        // FIXME: Cook the string here.
        red.Green.Text


/// Boolean node in the syntax tree.
and BoolVal internal (red: SyntaxToken) =

    inherit ConstantValue(red)

    member public x.Value =
        x.Text.StartsWith("#t")


/// Character node in the syntax tree.
and CharVal internal (red: SyntaxToken) =

    inherit ConstantValue(red)

    member public x.Value =
        // TODO: Cook this character
        x.Text[1]

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

    // FIXME: This should return a seq of expression, not raw syntax noddes
    member public _.Body =
        red.Children()
        |> Seq.choose Expression.TryCast

    member public _.ClosingParen =
        red.ChildrenWithTokens()
        |> Seq.choose (NodeOrToken.asToken)
        |> Seq.tryFind (tokenOfKind AstKind.CLOSE_PAREN)


/// Symbolic identifier node. This wraps an indentifier token when it is used
/// as a symbol in the source text.
and Symbol internal (red: SyntaxNode) =

    inherit Expression(red)

/// This is a self-evaluating expression that contains a sngle constant datum.
and Constant internal (red: SyntaxNode) =

    inherit Expression(red)

    member public _.Value =
        red.ChildrenWithTokens()
        |> Seq.choose (NodeOrToken.asToken)
        |> Seq.tryExactlyOne
        |> Option.bind (ConstantValue.TryCast)

/// Quoted datum node
and Quoted internal (red: SyntaxNode) =

    inherit Expression(red)

/// Vector constant node
and Vec internal (red: SyntaxNode) =

    inherit Expression(red)

/// Byte Vector constant node
and ByteVec internal (red: SyntaxNode) =

    inherit Expression(red)

/// Expression value. This is the set of all expression types. Expressions can
/// be either a simple datum (`Constant`), an identifier `Symbol`, or a comple
/// `Form` datum.
and Expression internal (red: SyntaxNode) =

    static member TryCast (node: SyntaxNode) =
        match node.Kind |> greenToAst with
        | AstKind.FORM -> Some(new Form(node) :> Expression)
        | AstKind.SYMBOL -> Some(new Symbol(node))
        | AstKind.CONSTANT -> Some(new Constant(node))
        | _ -> None

/// Root AST type for script expressions.
type ScriptProgram internal (red: SyntaxNode) =

    inherit AstNode(red)

    member _.Body =
        red.Children()
        |> Seq.choose (Expression.TryCast)
        |> Seq.tryExactlyOne

    static member TryCast(red: SyntaxNode) =
        if red.Kind = (AstKind.PROGRAM |> astToGreen) then
            Some(new ScriptProgram(red))
        else
            None

/// Wrapper type to represent an entire parsed program. This represents the
/// contents of a single compilation unit.
type Program internal (red: SyntaxNode) =

    inherit AstNode(red)

    member _.Body = red.Children() |> Seq.choose (Expression.TryCast)

    static member TryCast(red: SyntaxNode) =
        if red.Kind = (AstKind.PROGRAM |> astToGreen) then
            Some(new Program(red))
        else
            None
