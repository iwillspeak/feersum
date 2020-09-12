(display (((lambda a (lambda () a)) 123)))
(display ((lambda () (define a 100) (define (b) a) a)))