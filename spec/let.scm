(import (scheme base)
    (scheme write))
(define (foo a)
    (let ((b a)
        (c 100))
        (display (+ a b))
        (- b c)))
(foo 123) ; ~> 23