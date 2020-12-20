(define-syntax test-macro
    (syntax-rules ::: (test)
        ((_) 'nowt)
        ((_ test things :::) (list things :::))))

(test-macro)
(test-macro test 1 2 3)
(test-macro fail this one)

(let-syntax
    ((another (syntax-rules ()
        ((_ a ...) '(a ...))
        ; we expect a diagnostic for both
        ; these malformed dotted forms
        ((. _) (. "test"))
        )))
    (another 1 2 3))