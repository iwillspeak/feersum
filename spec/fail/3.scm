; expectfail
(define n 1)
(lambda ()
        (set! n (+ 1 n) ; note the `)` is on the next line. This `set!` is malformed
        n))