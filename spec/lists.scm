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

(display
  (list-ref ’(a b c d) 2)) ; =⇒ c
(newline)

(display
  (let ((ls (list ’one ’two ’five!)))
    (list-set! ls 2 ’three)
    ls) ; =⇒ (one two three)
  )(newline)

(display (eq? (list-copy test-list) test-list))(newline)
(display (list-copy 2))(newline)


(display (memq ’a ’(a b c)))(newline) ; -> (a b c)
(display (memq ’b ’(a b c)))(newline) ; -> (b c)
(display (memq ’a ’(b c d)))(newline) ; -> #f
(display (memq (list ’a) ’(b (a) c)))(newline) ; -> #f
(display (member (list ’a)
  ’(b (a) c)))(newline) ; -> ((a) c)
(display (member #\B
  ’(#\a #\b #\c)
  char-ci=?))(newline) ; -> (#\b #\c)
(display (memq 101 ’(100 101 102)))(newline) ; -> unspecified
(display (memv 101 ’(100 101 102)))(newline) ; -> (101 102)

(define e ’((a 1) (b 2) (c 3)))
(display (assq ’a e))(newline) ; -> (a 1)
(display (assq ’b e))(newline) ; -> (b 2)
(display (assq ’d e))(newline) ; -> #f
(display (assq (list ’a) ’(((a)) ((b)) ((c)))))(newline) ; -> #f
(display (assoc (list ’a) ’(((a)) ((b)) ((c)))))(newline) ; -> ((a))
(display (assoc 2.0 ’((1 1) (2 4) (3 9)) =))(newline) ; -> (2 4)
(display (assq 5 ’((2 3) (5 7) (11 13))))(newline) ; -> unspecified
(display (assv 5 ’((2 3) (5 7) (11 13))))(newline) ; -> (5 7)
