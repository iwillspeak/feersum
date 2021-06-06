#|
    The FACT procedure computes the factorial of a non-negative integer.
|#
(define fact
    (lambda (n)
        ; ; ~~~ Remove these when builtins are properly supported
        ; (define (- x) x)
        ; (define (= y) y)
        ; (define (* z) z)
        ; ; ~~~~ Remove when scopes are supported properly
        ; (define (fact x) x)

        (if (= n 0)
            #;(= n 1)
            1        ;Base case: return 1
            (* n (fact (- n 1))))))

(display (fact 3)) ; -> 6