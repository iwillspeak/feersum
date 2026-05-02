module TextTests

open Feersum.CompilerServices.Text
open Xunit

[<Fact>]
let ``text document for empty string`` () =
    let doc = TextDocument.fromParts "hello.scm" ""

    Assert.Equal(TextPoint.FromParts("hello.scm", 1, 1), TextDocument.offsetToPoint doc 0u)

[<Fact>]
let ``text document points`` () =
    let body =
        @"(define hello 123)
(let ((id (lammbda (x) x))) (id (hello 123)))

;; Blank lines and comments

990
"

    let doc = TextDocument.fromParts "test.scm" body

    let c (line, col) off =
        Assert.Equal(TextPoint.FromParts("test.scm", line, col), TextDocument.offsetToPoint doc off)

    c (1, 1) 0u
    c (6, 1) (uint32 (body.LastIndexOf("990")))
    c (7, 1) (uint32 body.Length)
    c (2, 2) (uint32 (body.IndexOf("let")))
    c (4, 10) (uint32 (body.IndexOf("lines and comments")))
