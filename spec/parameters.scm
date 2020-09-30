(define (foo a b)
    b)
(define (bar a b c)
    a)
; should return 101
(bar (foo 123 101) 78 90)