(import (scheme base)
  (scheme write))

(define (foo a)
  (define (bar b)
    (+ a b))
  bar)

(display ((foo 123) 4))
