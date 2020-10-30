;; Tests for the Equivalence Predicates (section 6.1 of R7RS).

;; Trivial equivalence
(display (list

    (eqv? ’a ’a) ; ~> #t
    (eqv? ’a ’b) ; ~> #f
    (eqv? 2 2) ; ~> #t
    (eqv? 2 2.0) ; ~> #f Should be false, but we don't do exact numbers :-/
    (eqv? ’() ’()) ; ~> #t
    (eqv? 100000000 100000000) ; ~> #t
    ; (eqv? 0.0 +nan.0) ; ~> #f
    (eqv? (cons 1 2) (cons 1 2)) ; ~> #f
    (eqv? (lambda () 1)(lambda () 2)) ; ~> #f
    (let ((p (lambda (x) x)))(eqv? p p)) ; ~> #t
    (eqv? #f ’nil) ; ~> #f

    ))(newline)

;; implementaiton specified restults
(display (list

    (eqv? "" "") ; ~> unspecified
    (eqv? ’#() ’#()) ; ~> unspecified
    (eqv? (lambda (x) x)(lambda (x) x)) ; ~> unspecified
    (eqv? (lambda (x) x)(lambda (y) y)) ; ~> unspecified
    ; (eqv? 1.0e0 1.0f0) ; ~> unspecified
    ; (eqv? +nan.0 +nan.0) ; ~> unspecified

    ))(newline)

;; Lambdas that capture state should be different for each call
(define gen-counter
    (lambda ()
        (let ((n 0))
            (lambda ()
                (set! n (+ n 1)) n))))
(display (let ((g (gen-counter)))
    (eqv? g g))) ; ~> #t
(display (eqv? (gen-counter) (gen-counter))) ; ~> #f
(newline)