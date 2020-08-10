(define x (lambda y
    (lambda z 100)))
(define y (lambda y
    (lambda z 23123)))
(define z (lambda y 123))
(define a (lambda (a b c) 20))
(define b (lambda (a b . c) 1234))
;(x) ; -> 100