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

;; The `eq?` Predicate is allowed to just do a pointer equivalence test
(display
 (list
  (eq? ’a ’a) ; -> #t
  (eq? ’(a) ’(a)) ; -> unspecified
  (eq? (list ’a) (list ’a)) ; -> #f
  (eq? "a" "a") ; -> unspecified
  (eq? "" "") ; -> unspecified
  (eq? ’() ’()) ; -> #t
  (eq? 2 2) ; -> unspecified
  (eq? #\A #\A) ; -> unspecified
  (eq? car car) ; -> #t
  (let ((n (+ 2 3)))
	(eq? n n)) ; -> unspecified
  (let ((x ’(a)))(eq? x x)) ; -> #t
  (let ((x ’#()))(eq? x x)) ; -> #t
  (let ((p (lambda (x) x)))(eq? p p)) ; -> #t
  ))(newline)

;; The `equals?` predicate should recursively calculate equivalence. For
;; our implementation this is close to `object.Equals`.
(display
 (list
  (equal? ’a ’a) ; -> #t
  (equal? ’(a) ’(a)) ; -> #t
  (equal? ’(a (b) c)’(a (b) c)) ; -> #t
  (equal? "abc" "abc") ; -> #t
  (equal? 2 2) ; -> #t
  (equal? (make-vector 5 ’a)(make-vector 5 ’a)) ; -> #t
  ; We don't support quote references yet
  ; (equal? ’#1=(a b . #1#)’#2=(a b a b . #2#)) ; -> #t
  (equal? (lambda (x) x)(lambda (y) y)) ; -> unspecified
  ))(newline)
