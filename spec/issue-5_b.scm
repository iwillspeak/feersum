(import (scheme base))
(define (check n)
    (letrec ((odd? (lambda (m)
                (if (= m 0)
                    #f
                    (even? (- m 1)))))
            (even? (lambda (m)
                (if (= m 0)
                    #t
                    (odd? (- m 1))))))
        (even? n)))

(display (check 54))(newline)
(display (check 7))(newline)
(check 123)