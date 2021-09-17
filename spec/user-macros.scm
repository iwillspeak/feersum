(import (scheme base)
    (scheme write))

(define-syntax foo
    (syntax-rules ()
        ((_ 123) '(1 2 3))
        ((_) "bar")))
(display (list
    (foo 123)
    (foo)))
(newline)

(let ()
    (define-syntax foo
        (syntax-rules (bar)
            ((_ bar) "foobar")
            ((_ baz) "bizbaz")))
    (display (list (foo bar) (foo b)))
    (newline))

(display
    (let-syntax
        ((foo (syntax-rules ()
            ((foo _) '(inner foo)))))
        (foo "123")))(newline)

; Should be the first `foo` again...
(display (foo 123))(newline)

