module LexTests

open Xunit

open Feersum.Syntax

/// Grab the kind from a syntax token pair.
let private getKind token =
    let (kind, _) = token
    kind

/// Grab the value from a syntax token pair.
let private getValue token =
    let (_, value) = token
    value

[<Fact>]
let ``Empty input text always returns end of file`` () =
    let lexer = Lexer("")

    Assert.Equal(TokenKind.EndOfFile, lexer.Current |> getKind)
    lexer.Bump()
    Assert.Equal(TokenKind.EndOfFile, lexer.Current |> getKind)
    lexer.Bump()
    Assert.Equal(TokenKind.EndOfFile, lexer.Current |> getKind)

[<Theory>]
[<InlineData(" ", TokenKind.Whitespace)>]
[<InlineData("  ", TokenKind.Whitespace)>]
[<InlineData("\t \t", TokenKind.Whitespace)>]
[<InlineData("\n", TokenKind.Whitespace)>]
[<InlineData("\r\n", TokenKind.Whitespace)>]
[<InlineData("             \r           ", TokenKind.Whitespace)>]
[<InlineData("(", TokenKind.OpenBracket)>]
[<InlineData(")", TokenKind.CloseBracket)>]
[<InlineData("`", TokenKind.Quote)>]
[<InlineData("'", TokenKind.Quote)>]
[<InlineData("’", TokenKind.Quote)>]
[<InlineData(",", TokenKind.Unquote)>]
[<InlineData(".", TokenKind.Dot)>]
[<InlineData(";", TokenKind.Comment)>]
[<InlineData("; anything", TokenKind.Comment)>]
[<InlineData(";;; anything", TokenKind.Comment)>]
[<InlineData("#||#", TokenKind.Comment)>]
[<InlineData("#| anything |#", TokenKind.Comment)>]
[<InlineData("#| \n |#", TokenKind.Comment)>]
[<InlineData("#| || || |#", TokenKind.Comment)>]
[<InlineData("#| #| double |# |#", TokenKind.Comment)>]
[<InlineData("#|  unterminated", TokenKind.Error)>]
[<InlineData("#|  unterminated |", TokenKind.Error)>]
[<InlineData("#", TokenKind.Error)>]
[<InlineData("foo", TokenKind.Identifier)>]
[<InlineData("f123", TokenKind.Identifier)>]
[<InlineData("ffoo.bar", TokenKind.Identifier)>]
[<InlineData("~", TokenKind.Identifier)>]
let ``Lexer lex single token`` (token, kind) =
    let lexer = Lexer(token)

    Assert.Equal(kind, lexer.Current |> getKind)
    Assert.Equal(kind, lexer.Current |> getKind)
    Assert.Equal(token, lexer.Current |> getValue)
    lexer.Bump()
    Assert.Equal(TokenKind.EndOfFile, lexer.Current |> getKind)
