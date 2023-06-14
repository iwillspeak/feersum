module LexTests

open Xunit

open Feersum.CompilerServices.Syntax
open Feersum.CompilerServices.Syntax.Lex
open Feersum.CompilerServices.Text

let private p name line col =
    TextPoint.FromParts(name, line, col) |> TextLocation.Point

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
    let tokens = tokenise "" "test.scm"

    Assert.Empty(tokens)

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
[<InlineData("#;", TokenKind.DatumCommentMarker)>]
[<InlineData("-", TokenKind.Identifier)>]
[<InlineData("+", TokenKind.Identifier)>]
[<InlineData("-.", TokenKind.Identifier)>]
[<InlineData("..", TokenKind.Identifier)>]
[<InlineData("...", TokenKind.Identifier)>]
[<InlineData(".-", TokenKind.Identifier)>]
[<InlineData("-@", TokenKind.Identifier)>]
[<InlineData("1", TokenKind.Number)>]
[<InlineData("101", TokenKind.Number)>]
[<InlineData("1234567890", TokenKind.Number)>]
[<InlineData("\"", TokenKind.Error)>]
[<InlineData("\"\\", TokenKind.Error)>]
[<InlineData("\"\\\"", TokenKind.Error)>]
[<InlineData("\"\"", TokenKind.String)>]
[<InlineData("\" some \\\" test\\\\\"", TokenKind.String)>]
[<InlineData("||", TokenKind.Identifier)>]
[<InlineData("|", TokenKind.Error)>]
[<InlineData("|\\", TokenKind.Error)>]
[<InlineData("|\\|", TokenKind.Error)>]
[<InlineData("| some \\| test\\\\|", TokenKind.Identifier)>]
[<InlineData("#\\x101", TokenKind.Character)>]
[<InlineData("#\\x", TokenKind.Character)>]
[<InlineData("#\\a", TokenKind.Character)>]
[<InlineData("#\\alarm", TokenKind.Character)>]
[<InlineData("#\\ ", TokenKind.Character)>]
[<InlineData("#(", TokenKind.VectorPrefix)>]
[<InlineData("#u8(", TokenKind.BytevectorPrefix)>]
[<InlineData("#t", TokenKind.Boolean)>]
[<InlineData("#f", TokenKind.Boolean)>]
[<InlineData("#true", TokenKind.Boolean)>]
[<InlineData("#false", TokenKind.Boolean)>]
[<InlineData(".100", TokenKind.Number)>]
[<InlineData("+.345", TokenKind.Number)>]
[<InlineData("-.67", TokenKind.Number)>]
[<InlineData("+89", TokenKind.Number)>]
[<InlineData("-100", TokenKind.Number)>]
[<InlineData("+89.0", TokenKind.Number)>]
[<InlineData("-100.", TokenKind.Number)>]
[<InlineData("-..10", TokenKind.Identifier)>]
[<InlineData("+-.0", TokenKind.Identifier)>]
let ``Lexer lex single token`` (token, kind) =
    let tokens = Lex.tokenise token "test.scm"

    let (line, col) =
        token
        |> Seq.fold (fun (line, col) char -> if char = '\n' then (line + 1, 0) else (line, col + 1)) (1, 0)

    Assert.Equal(
        [ { Kind = kind
            Lexeme = token
            Location = (p "test.scm" line col) } ],
        tokens
    )

[<Fact>]
let ``Lexer happy path`` () =

    let tokens =
        Lex.tokenise "(display #| hello |# world)" "test.scm"
        |> Seq.map (fun token -> token.Kind, token.Lexeme)

    Assert.Equal(
        [ TokenKind.OpenBracket, "("
          TokenKind.Identifier, "display"
          TokenKind.Whitespace, " "
          TokenKind.Comment, "#| hello |#"
          TokenKind.Whitespace, " "
          TokenKind.Identifier, "world"
          TokenKind.CloseBracket, ")" ],
        tokens
    )
