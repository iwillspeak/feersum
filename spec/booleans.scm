(import (scheme base))
;; Boolean literals and truthiness
(display (list
    #f
    #false
    #t
    #true
    (eq? #t #true)
    (eqv? #t #true)
    (equal? #t #true)
    (eq? #f #false)
    (eqv? #f #false)
    (equal? #f #false)
    (if (eq? #f #true)
        'fail
        'pass)
    (if (equal? #t #f)
        'fail
        'pass)
    (if #f
        'fail
        'pass)
    (if #t
        'pass
        'fail)
    (if '()
        'pass
        'fail)
    (if 'something
        'pass
        'fail)
    ))(newline)
;; Quoting of boolean values does nothing to them
(display (list
    '#f
    '#true))(newline)
;; Boolean procedures, we don't have many of them builtin
(display (list
    (not #t) ; -> #f
    (not 3) ; -> #f
    (not (list 3)) ; -> #f
    (not #f) ; -> #t
    (not ’()) ; -> #f
    (not (list)) ; -> #f
    (not ’nil) ; -> #f

    (boolean? #f) ; -> #t
    (boolean? 0) ; -> #f
    (boolean? ’()) ; -> #f
    
    (boolean=? #t #t #t)
    (boolean=? #t #f #t)
    (boolean=? #f #f #f #f #t)
    (boolean=? #f #f #f #f)
    (boolean=? #f #f)
    (boolean=? #f 0)
    (boolean=? #f '())
    (boolean=? (list 123) 21 #t)

    ))(newline)