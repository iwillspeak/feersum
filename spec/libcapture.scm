(define-library (lib capture)
    (export testit)
    (begin
        (define testit "broken")
        (let ((replacement "pass"))
            (set! testit replacement))))
(import (lib capture)
    (scheme write))
(display testit)(newline)