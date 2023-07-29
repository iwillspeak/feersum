(define world)
(define (hello) (world))
(set! world (lambda () (hello)))
(hello)
