(import (scheme write)
    (scheme base))

(define-syntax bug
    (syntax-rules ()
        ((bug (a ...) (b ...)) (cons (list a ...) (list b ...)))))
(define-syntax zip
    (syntax-rules ()
        ((bug (a ...) (b ...)) (list (cons a b) ...))))

(display (list
    (bug (1 2 3) (4 5)) ; -> '((1 2 3) 4 5)
    (zip (1 2) (3 4))   ; -> '((1 . 3) (2 . 4))
    ))(newline)
