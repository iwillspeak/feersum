module SemanticTokensTests

open Xunit
open Feersum.LanguageServer

// -- Helpers ------------------------------------------------------------------

/// A decoded representation of one LSP semantic token (5 uint32 values).
[<Struct>]
type private DecodedToken =
    { DeltaLine: uint32
      DeltaCol: uint32
      Length: uint32
      TokenType: uint32
      Modifiers: uint32 }

/// Decode a flat uint32 array (5 values per token) into DecodedToken records.
let private decode (data: uint32[]) =
    if data.Length % 5 <> 0 then
        failwithf "Token data length %d is not a multiple of 5" data.Length

    data
    |> Array.chunkBySize 5
    |> Array.map (fun c ->
        { DeltaLine = c[0]
          DeltaCol = c[1]
          Length = c[2]
          TokenType = c[3]
          Modifiers = c[4] })

let private tokenize src =
    SemanticTokenizer.tokenize "test.scm" src |> decode

let private CommentType = uint32 (fst SemanticTokenizer.tokenComment)
let private KeywordType = uint32 (fst SemanticTokenizer.tokenKeyword)
let private NumberType = uint32 (fst SemanticTokenizer.tokenNumber)
let private OperatorType = uint32 (fst SemanticTokenizer.tokenOperator)
let private StringType = uint32 (fst SemanticTokenizer.tokenString)
let private VariableType = uint32 (fst SemanticTokenizer.tokenVariable)
let private EnumType = uint32 (fst SemanticTokenizer.tokenEnum)

// -- Legend -------------------------------------------------------------------

[<Fact>]
let ``token type names match expected legend order`` () =
    Assert.Equal(SemanticTokenizer.tokens.Length, SemanticTokenizer.tokenTypeNames.Length)

    for token in SemanticTokenizer.tokens do
        let name = SemanticTokenizer.tokenTypeNames[int (fst token)]
        let expectedName = snd token
        Assert.Equal(expectedName, name)

// -- Classification -----------------------------------------------------------

[<Fact>]
let ``empty source produces no tokens`` () = Assert.Empty(tokenize "")

[<Fact>]
let ``line comment is classified as comment`` () =
    let t = Assert.Single(tokenize "; hello")
    Assert.Equal(CommentType, t.TokenType)
    Assert.Equal(0u, t.DeltaLine)
    Assert.Equal(0u, t.DeltaCol)
    Assert.Equal(7u, t.Length)

[<Fact>]
let ``block comment is classified as comment`` () =
    let t = Assert.Single(tokenize "#| block comment |#")
    Assert.Equal(CommentType, t.TokenType)
    Assert.Equal(19u, t.Length)

[<Fact>]
let ``number literal is classified as number`` () =
    let t = Assert.Single(tokenize "42")
    Assert.Equal(NumberType, t.TokenType)
    Assert.Equal(2u, t.Length)

[<Fact>]
let ``string literal is classified as string`` () =
    let t = Assert.Single(tokenize "\"hello\"")
    Assert.Equal(StringType, t.TokenType)
    Assert.Equal(7u, t.Length)

[<Fact>]
let ``boolean #t is classified as keyword`` () =
    let t = Assert.Single(tokenize "#t")
    Assert.Equal(EnumType, t.TokenType)

[<Fact>]
let ``boolean #f is classified as keyword`` () =
    let t = Assert.Single(tokenize "#f")
    Assert.Equal(EnumType, t.TokenType)

[<Fact>]
let ``identifier is classified as variable`` () =
    let t = Assert.Single(tokenize "foo")
    Assert.Equal(VariableType, t.TokenType)

[<Fact>]
let ``quote mark is classified as operator`` () =
    let tokens = tokenize "'x"
    Assert.Equal(2, tokens.Length)
    Assert.Equal(OperatorType, tokens[0].TokenType)
    Assert.Equal(VariableType, tokens[1].TokenType)

[<Fact>]
let ``multi-line string is skipped`` () =
    Assert.Empty(tokenize "\"line1\nline2\"")

// -- Delta encoding -----------------------------------------------------------

[<Fact>]
let ``first token has deltaLine 0 and deltaCol 0`` () =
    let t = Assert.Single(tokenize "42")
    Assert.Equal(0u, t.DeltaLine)
    Assert.Equal(0u, t.DeltaCol)

[<Fact>]
let ``tokens on same line have deltaLine 0 and relative deltaCol`` () =
    let tokens = tokenize "1 2"
    Assert.Equal(2, tokens.Length)
    Assert.Equal(0u, tokens[0].DeltaLine)
    Assert.Equal(0u, tokens[0].DeltaCol)
    // Second token is at col 2; relative to col 0 -> deltaCol 2.
    Assert.Equal(0u, tokens[1].DeltaLine)
    Assert.Equal(2u, tokens[1].DeltaCol)

[<Fact>]
let ``token on next line has deltaLine 1 and absolute col`` () =
    let tokens = tokenize "1\n2"
    Assert.Equal(2, tokens.Length)
    Assert.Equal(1u, tokens[1].DeltaLine)
    Assert.Equal(0u, tokens[1].DeltaCol)

[<Fact>]
let ``token on indented next line carries absolute col`` () =
    let tokens = tokenize "1\n  2"
    Assert.Equal(2, tokens.Length)
    Assert.Equal(1u, tokens[1].DeltaLine)
    Assert.Equal(2u, tokens[1].DeltaCol)
