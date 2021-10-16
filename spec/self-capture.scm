(import (scheme base)
    (scheme write))

(define (foo)   ;  [], [Local 0]            -> [], [Local 0]
  (define (bar x)  ; [Local 0], [Arg 0]     -> [Environment 0], [Arg 0]
    (if (< x 10)
      x
      (bar (- x 1))))
  (bar 10))

(display (foo))(newline)
