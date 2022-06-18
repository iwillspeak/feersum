namespace Feersum.CompilerServices.Syntax

open System.IO

open Firethorn.Green
open Firethorn.Red
open Feersum.CompilerServices.Diagnostics

module private Diagnostics =

    let parseError = DiagnosticKind.Create DiagnosticLevel.Error 10 "Parse error"

module Parse =

    open Tree

    /// Type to represent parse results.
    type public ParseResult<'a> =
        { Diagnostics: Diagnostic list
          Root: 'a }

    module ParseResult =

        /// Check a parse result for errors
        let public hasErrors result = result.Diagnostics |> hasErrors

        /// Map the results of a parse
        let public map mapper result =
            { Diagnostics = result.Diagnostics
              Root = result.Root |> mapper }

    /// Type to represent different kinds of programs we can read
    type public ReadMode =
        | Program
        | Script

    /// Parser Type
    ///
    /// Class type to hold state when parsing. This is not intended to be used
    /// directly. The main parser API is via the `read*` functions.
    type private Parser(lexer: Lexer) =

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
            errors <-
                Diagnostic.Create Diagnostics.parseError lexer.Position message
                :: errors

        member private self.LookingAt(tokenKind: TokenKind) = self.CurrentKind = tokenKind

        member private self.LookingAtAny(kinds: TokenKind list) = List.contains self.CurrentKind kinds

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

        member private self.SkipAtmosphere() =
            while self.LookingAtAny(
                [ TokenKind.Whitespace
                  TokenKind.Comment ]
            ) do
                self.Bump(AstKind.ATMOSPHERE)

        member private self.ParseIdentifier() =
            builder.StartNode(AstKind.SYMBOL |> astToGreen)
            self.Bump(AstKind.IDENTIFIER)
            builder.FinishNode()

        member private self.ParseQuote() =
            builder.StartNode(AstKind.QUOTED_DATUM |> astToGreen)
            self.Expect(TokenKind.Quote, AstKind.QUOTE)
            self.ParseExpr()
            builder.FinishNode()

        member private self.ParseAtom() =
            match self.CurrentKind with
            | TokenKind.Identifier -> self.ParseIdentifier()
            | TokenKind.Quote -> self.ParseQuote()
            | _ -> self.ParseConstant()

        member private self.ParseForm() =
            builder.StartNode(AstKind.FORM |> astToGreen)
            self.Expect(TokenKind.OpenBracket, AstKind.OPEN_PAREN)

            while not
                  <| self.LookingAtAny(
                      [ TokenKind.EndOfFile
                        TokenKind.CloseBracket ]
                  ) do
                self.ParseExpr()

            self.Expect(TokenKind.CloseBracket, AstKind.CLOSE_PAREN)
            builder.FinishNode()

        member private self.ParseExpr() =
            match self.CurrentKind with
            | TokenKind.OpenBracket -> self.ParseForm()
            | _ -> self.ParseAtom()

            self.SkipAtmosphere()

        member private _.Finalise(rootKind: AstKind) =
            let root =
                builder.BuildRoot(rootKind |> astToGreen)
                |> SyntaxNode.CreateRoot

            { Diagnostics = errors; Root = root }

        /// Parse Program
        ///
        /// Read a sequence of expressions as a single program.
        member self.ParseProgram() =
            self.SkipAtmosphere()

            while not <| self.LookingAt(TokenKind.EndOfFile) do
                self.ParseExpr()

            self.Expect(TokenKind.EndOfFile, AstKind.EOF)
            self.Finalise(AstKind.PROGRAM)

        /// Parse Expression
        ///
        /// Reads a single expression from the lexer
        member self.ParseExpression() =
            self.SkipAtmosphere()

            self.ParseExpr()

            self.Expect(TokenKind.EndOfFile, AstKind.EOF)
            self.Finalise(AstKind.EXPR_PROGRAM)

    let readRaw mode name line =
        let parser = Parser(Lexer(line, name))

        match mode with
        | Program -> parser.ParseProgram()
        | Script -> parser.ParseExpression()

    let readProgram name line =
        readRaw Program name line
        |> ParseResult.map (fun x -> new Program(x))

    /// Read a single expression from the named input text
    let readExpr1 name line =
        readRaw Script name line
        |> ParseResult.map (fun x -> new ScriptProgram(x))

    /// Read a single expression from the input text
    let readExpr = readExpr1 "repl"

    /// Read an expression from source code on disk
    let parseFile path =
        async {
            let! text = File.ReadAllTextAsync(path) |> Async.AwaitTask
            return readProgram path text
        }
