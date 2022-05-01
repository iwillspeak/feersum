namespace Feersum.CompilerServices.Syntax

open Firethorn
open Firethorn.Green
open Firethorn.Red
open Feersum.CompilerServices.Diagnostics

module ParseNew =

    open TreeNew

    /// Parser Type
    ///
    /// Class type to hold state when parsing. This is not intended to be used
    /// directly. The main parser API is via the `read*` functions.
    type Parser(lexer: Lexer) =

        let builder = GreenNodeBuilder()
        let mutable errors = []

        let getText token =
            let (_, text) = token
            text

        let getKind token =
            let (kind, _) = token
            kind

        member private _.CurrentKind = lexer.Current |> getKind

        member private _.CurrentText = lexer.Current |> getText

        member private _.ErrAtPoint(message: string) =
            errors <- Diagnostic.Create lexer.Position message :: errors

        member private self.LookingAt(tokenKind: TokenKind) = self.CurrentKind = tokenKind

        member private self.Bump(kind: AstKind) =
            builder.Token(kind |> astToGreen, self.CurrentText)
            lexer.Bump()

        member private self.Expect(tokenKind: TokenKind, nodeKind: AstKind) =
            if self.LookingAt(tokenKind) then
                self.Bump(nodeKind)
            else
                sprintf "Expected %A, got %A" tokenKind self.CurrentKind
                |> self.ErrAtPoint

                builder.Token(AstKind.ERROR |> astToGreen, "")

        member private self.ParseErr(message: string) =
            self.ErrAtPoint(message)
            self.Bump(AstKind.ERROR)

        member private self.ParseConstant() =
            builder.StartNode(AstKind.CONSTANT |> astToGreen)

            match self.CurrentKind with
            | TokenKind.String -> self.Bump(AstKind.STRING)
            | TokenKind.Number -> self.Bump(AstKind.NUMBER)
            | TokenKind.Boolean -> self.Bump(AstKind.BOOLEAN)
            | TokenKind.Character -> self.Bump(AstKind.CHARACTER)
            | _ ->
                sprintf "Unexpected token %A" self.CurrentKind
                |> self.ParseErr

            builder.FinishNode()

        member private self.ParseIdentifier() =
            builder.StartNode(AstKind.SYMBOL |> astToGreen)
            self.Bump(AstKind.IDENTIFIER)
            builder.FinishNode()

        member private self.ParseAtom() =
            match self.CurrentKind with
            | TokenKind.Identifier -> self.ParseIdentifier()
            | _ -> self.ParseConstant()

        member private self.ParseExpr() =
            match self.CurrentKind with
            | _ -> self.ParseAtom()

        member private self.Finalise(rootKind: AstKind) =
            let root =
                builder.BuildRoot(rootKind |> astToGreen)
                |> SyntaxNode.CreateRoot

            { Errors = errors; Root = root }

        /// Parse Program
        ///
        /// Read a sequence of expressions as a single program.
        member self.ParseProgram() =
            while not <| self.LookingAt(TokenKind.EndOfFile) do
                self.ParseExpr()

            self.Expect(TokenKind.EndOfFile, AstKind.EOF)
            self.Finalise(AstKind.PROGRAM)

        /// Parse Expression
        ///
        /// Reads a single expression from the lexer
        member self.ParseExpression() =
            self.ParseExpr()

            self.Expect(TokenKind.EndOfFile, AstKind.EOF)
            self.Finalise(AstKind.EXPR_PROGRAM)


    let readExpr1 name line : ParseResult =
        let parser = Parser(Lexer(line, name))
        parser.ParseExpression()
