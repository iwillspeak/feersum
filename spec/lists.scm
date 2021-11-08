(import (scheme base)
    (scheme write))

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

(display (append ’(x) ’(y)))(newline) ; -> (x y)
(display (append ’(a) ’(b c d)))(newline) ; -> (a b c d)
(display (append ’(a (b)) ’((c))))(newline) ; -> (a (b) (c))
(display (append ’(a b) ’(c . d)))(newline) ; -> (a b c . d) ;; FIXME: quoted pairs!
(display (append ’(a b) (cons 'c 'd)))(newline) ; -> (a b c . d)
(display (append ’() ’a))(newline) ; -> a

(display (reverse ’(a b c)))(newline) ; -> (c b a)
(display (reverse ’(a (b c) d (e (f)))))(newline) ; -> ((e (f)) d (b c) a)

(define test-list '(a b c d e f g))
(display (list-tail test-list 1))(newline)
(display (list-tail test-list 0))(newline)
(display (list-tail test-list 6))(newline)
