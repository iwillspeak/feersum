; Simple capture test: an inner lambda captures a variable from an outer lambda
; The Lower pass should rewrite the reference to x in the inner lambda
; to use environment slot indexing instead of direct local reference

(import (scheme base))

(define (make-adder n)
  (lambda (x) (+ x n)))

(let ((f (make-adder 10)))
  (f 5))
