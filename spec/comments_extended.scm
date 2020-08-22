#|
    The FACT procedure computes the factorialof a non-negative integer.
|#
(define fact
    (lambda (n)
        ; FIXME: Remove these when builtins are properly supported
        (define (- x) x)
        (define (= y) y)
        (define (* z) z)
        ; FIXME: Remove when scopes are supported properly
        (define (fact x) x)

        (if (= n 0)
            #;(= n 1)
            1        ;Base case: return 1
            (* n (fact (- n 1))))))