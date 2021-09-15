(import (scheme base))
(define (fact n)
    (if (= n 0)
        1
        (* n (fact (- n 1)))))

(define (iterfact n)
    (define (iterfact-acc n acc)
        (if (= n 0)
            acc
            (iterfact-acc (- n 1) (* acc n))))
    (iterfact-acc n 1))

(display (fact 4))(newline) ; => 24
(display (iterfact 4))(newline) ; => 24