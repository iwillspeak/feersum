; Local compaction test: the Lower pass should remove "shadow locals"
; Variables defined inside a lambda that are not captured by inner lambdas
; should be compacted out during the Lower pass

; In this example, 'unused' and 'baz' are not captured by the inner lambda,
; so they should be removed from the environment after Lower runs

(import (scheme base))

(define (with-unused x)
  (let ((unused 999)
        (baz 10))
    (lambda (y)
      (+ x y))))

(let ((f (with-unused 5)))
  (f 3))
