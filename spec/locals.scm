(import (scheme base)
    (scheme write))

(define (fact n)
    (if (= n 0)
        1
        (begin
            (define m (- n 1))
            (* n (fact m)))))

(display (fact 5)) 