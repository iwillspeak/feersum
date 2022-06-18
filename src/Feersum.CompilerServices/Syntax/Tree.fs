namespace Feersum.CompilerServices.Syntax

open Firethorn
open Firethorn.Green
open Firethorn.Red

module Tree =

    /// Node kind for each element in the raw tree.
    type AstKind =
        | ERROR = -1

        // nodes
        | EXPR_PROGRAM = 1
        | PROGRAM = 2
        | CONSTANT = 3
        | SYMBOL = 4
        | FORM = 5
        | QUOTED_DATUM = 6

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

    /// Convert an AST Kind to a raw `SyntaxKind`
    let astToGreen (kind: AstKind) = SyntaxKind(int kind)

    /// Convert a raw kind back to an `AstKind`
    let greenToAst =
        function
        | SyntaxKind kind -> enum<AstKind> kind

    /// Debug dump. Writes a debug representation of the tree to stdout.
    let dump = Debug.debugDump (Debug.mappedFormatter greenToAst)

    [<AutoOpen>]
    module private Utils =

        /// Predicate funtion to filter tokesn by AST Kind
        let tokenOfKind (kind: AstKind) (token: SyntaxToken) = token.Kind = (kind |> astToGreen)

    /// Root type in the AST Tree. All node types should inherit from this
    /// either directly or indeirectly .
    [<AbstractClass>]
    type AstItem(red: NodeOrToken<SyntaxNode, SyntaxToken>) =

        /// Get the Syntax range of the item
        member public _.SyntaxRange =
            red
            |> NodeOrToken.consolidate (fun n -> n.Range) (fun t -> t.Range)

        member _.Text =
            red
            |> NodeOrToken.consolidate (fun n -> n.Range) (fun t -> t.Range)

        override _.ToString() =
            red
            |> NodeOrToken.consolidate (fun n -> n.ToString()) (fun t -> t.ToString())

    [<AbstractClass>]
    type AstNode(red: SyntaxNode) =
        
        inherit AstItem(red |> Node)

        member public _.RawNode = red

    [<AbstractClass>]
    type AstToken(red: SyntaxToken) =
        
        inherit AstItem(red |> Token)

        member public _.RawToken = red

    /// Form syntax wrapper
    type Form(red: SyntaxNode) =

        inherit AstNode(red)

        member public _.OpeningParen =
            red.ChildrenWithTokens()
            |> Seq.choose (NodeOrToken.asToken)
            |> Seq.tryFind (tokenOfKind AstKind.OPEN_PAREN)

        member public _.Body = red.Children()

        member public _.ClosingParen =
            red.ChildrenWithTokens()
            |> Seq.choose (NodeOrToken.asToken)
            |> Seq.tryFind (tokenOfKind AstKind.CLOSE_PAREN)

        static member TryCast(red: SyntaxNode) : Option<Form> =
            if red.Kind = (AstKind.FORM |> astToGreen) then
                Some(new Form(red))
            else
                None

    /// Number node in the syntax tree.
    type Number(red: SyntaxToken) =

        inherit AstToken(red)

        static member TryCast(red: SyntaxToken) : Option<Number> =
            if red.Kind = (AstKind.NUMBER |> astToGreen) then
                Some(new Number(red))
            else
                None

        member public x.Value =
            match red.Green.Text |> System.Double.TryParse with
            | (true, value) -> value
            | _ -> 0.0

    type Symbol(red: SyntaxNode) =

        inherit AstNode(red)

        static member TryCast(red: SyntaxNode) =
            if (red.Kind = (AstKind.SYMBOL |> astToGreen)) then
                Some(new Symbol(red))
            else
                None

    type ConstantValue =
        | Number of Number
        | Error

    type Constant(red: SyntaxNode) =

        inherit AstNode(red)

        member public _.Value =
            red.ChildrenWithTokens()
            |> Seq.choose (NodeOrToken.asToken)
            |> Seq.tryExactlyOne
            |> Option.bind (Number.TryCast)
            |> Option.map (ConstantValue.Number)

        static member TryCast(red: SyntaxNode) =
            if red.Kind = (AstKind.CONSTANT |> astToGreen) then
                new Constant(red) |> Some
            else
                None

    type Expression =
        | Form of Form
        | Constant of Constant
        | Symbol of Symbol

    module Expression =
        let tryCast (node: SyntaxNode) =
            match node.Kind |> greenToAst with
            | AstKind.FORM -> Some(Expression.Form(new Form(node)))
            | AstKind.SYMBOL -> Some(Expression.Symbol(new Symbol(node)))
            | AstKind.CONSTANT -> Some(Expression.Constant(new Constant(node)))
            | _ -> None

    /// Root AST type for script expressions.
    type ScriptProgram(red: SyntaxNode) =

        inherit AstNode(red)

        member _.Body =
            red.Children()
            |> Seq.choose (Expression.tryCast)
            |> Seq.tryExactlyOne

    /// Wrapper type to represent an entire parsed program. This represents the
    /// contents of a single compilation unit.
    type Program(red: SyntaxNode) =

        inherit AstNode(red)

        member _.Body = red.Children() |> Seq.choose (Expression.tryCast)

        static member TryCast(red: SyntaxNode) =
            if red.Kind = (AstKind.PROGRAM |> astToGreen) then
                Some(new Program(red))
            else
                None
