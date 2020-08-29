(define (fact n)
    (if (= n 0)
        1
        (* n (fact (- n 1)))))

(display (fact 4)) ; => 24