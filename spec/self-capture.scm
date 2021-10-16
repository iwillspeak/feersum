(import (scheme base)
    (scheme write))

;; Captured Local Compaction (shadow locals) Test
;
; In this example `foo` has two locals. The first is the function `bar` and the
; second is the initial value `baz`. Once we have performed free variable
; analysis on the function we identify that `Local 0` (bar) is captured. We
; want the Lower pass to re-write `baz` from `Local 1` down to `Local 0` in this
; case, as well as re-writing the references to the captured value to the env.
;
;                    captures, locals -- Lower --> captures, locals                  
(define (foo)      ; [], [Local 0; Local 1]     -> [], [Local 0]
  (define (bar x)  ; [Local 0], [Arg 0]         -> [Environment 0], [Arg 0]
    (if (< x 10)
      x
      (bar (- x 1))))
  (define baz 10)
  (bar baz))

(display (foo))(newline)
