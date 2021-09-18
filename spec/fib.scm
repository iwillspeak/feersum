(import (scheme base)
    (scheme write))

; 1 1 2 3 5 8
; 1 2 3 4 5 6

(define (fib n)
    (define (fib-helper n last cur)
        (if (<= n 2)
            cur
            (fib-helper (- n 1) cur (+ cur last))))
    (fib-helper n 1 1))


(display (fib 40))(newline)