(import (scheme base)
    (scheme write))
(define (make-counter n)
    (lambda ()
        (set! n (+ 1 n))
        n))
(define from-ten (make-counter 10))
(define from-ton (make-counter 100))

(display (from-ten))
(display (from-ton))

;; This should return 102 + 12
(display (+ (from-ton) (from-ten)))
(+ (from-ton) (from-ten))