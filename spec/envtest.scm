(import (scheme base)
  (scheme write))

(define (foo a)
  (define (bar b)
    (+ a b))
  bar)

(display ((foo 123) 4))(newline)

(define (test a b)
  (define (do-stuff)
    (define (things c)
      (+ a b c))
    (things 12))
  (do-stuff))

(display (test 1 2))(newline)
