; Deep capture chain test: a long chain of nested lambdas each capturing
; from the levels above. This tests that Lower's context chain correctly
; threads capture information through many levels

(import (scheme base))

(define (make-deep-closure a)
  (lambda (b)
    (lambda (c)
      (lambda (d)
        (lambda (e)
          (lambda (f)
            (+ a b c d e f)))))))

(let ((l1 (make-deep-closure 1)))
  (let ((l2 (l1 2)))
    (let ((l3 (l2 3)))
      (let ((l4 (l3 4)))
        (let ((l5 (l4 5)))
          (l5 6))))))
