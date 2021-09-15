(import (scheme base))

(display (list
    (list 12 34 56)                            ; (12 34 56)
    (cdr (cdr (list "a" #\B #f 4.0)))          ; -> (#f 4.0)
    (car (cdr (car (list (list 1 2 3) 4 5))))  ; -> 2
    ))
(newline)

(define (rev list)
    (define (rev-with-acc list acc)
        (if (null? list)
            acc
            (rev-with-acc (cdr list) (cons (car list) acc))))
    (rev-with-acc list ()))

(display (rev (list 1 2 3 4 5)))
(newline)

(display (list? (cons "a" ()))) ; -> #t
(display (list? (cons "a" (list)))) ; -> #t
(display (list? (cons "a" "b"))) ; -> #f
(newline)

(display (pair? (cons "a" ()))) ; -> #t
(display (pair? (cons "a" (list)))) ; -> #t
(display (pair? (cons "a" "b"))) ; -> #t
(newline)
