(import (scheme base))
(define-syntax test-macro
    (syntax-rules ::: (test)
        ((_) 'nowt)
        ((_ test things :::) (list things :::))))

(test-macro)
(test-macro test 1 2 3)
(test-macro fail this one)

(let-syntax
    ((another (syntax-rules ()
        ((_ a ...) a))))
    (another 1 2 3))

; another is not in scope here - expect a diagnostic
(another 1 2 3)