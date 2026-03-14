; Nested capture test: multiple levels of lambdas capturing from parents
; The Lower pass must handle capture chains where each lambda level
; reintroduces values captured from its parent

(import (scheme base))

(define (level-1 a)
  (lambda (b)
    (lambda (c)
      (+ a b c))))

(let ((f1 (level-1 10)))
  (let ((f2 (f1 20)))
    (f2 30)))
