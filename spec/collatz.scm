(import (scheme base)
    (scheme write))

;; Modululs remainder
;;
;; Returns the modulus remainder of n/d
(define (mod n d)
    (if (< n d)
        n
        (mod (- n d) d)))

;; Find how long a given number takes to converge on 1
(define (collatz x)
    (define (collatz-internal x steps)
        (if (= x 1)
            steps
            (collatz-internal
                (if (= (mod x 2) 0)
                    (/ x 2)
                    (+ (* x 3) 1))
            (+ steps 1))))
    (collatz-internal x 0))

(display (collatz 17))(newline) ;; => 12
(display (collatz 1))(newline)  ;; => 0
(display (collatz 9))(newline)  ;; => 19
