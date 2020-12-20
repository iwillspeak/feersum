(define-syntax foo
    (syntax-rules ()
        ((_ 123) '(1 2 3))
        ((_) "bar")))
(display (list
    (foo 123)
    (foo)))
(newline)