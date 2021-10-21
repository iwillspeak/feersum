(import (scheme base)
    (scheme write))

(define (min x y)
    (if (< x y) x y))
(define (max x y)
    (if (> x y) x y))

;; Computes the greatest consecutive difference
(define (greatest-diff nums)
    (define (greatest-diff-helper nums low ret)
        (if (null? nums)
            ret
            (let* ((cur (car nums))
                (rest (cdr nums))
                (diff (- cur low))
                (low (min low cur))
                (ret (max diff ret)))
                (greatest-diff-helper rest low ret))))
    (greatest-diff-helper nums 100000 -1))


(display (greatest-diff (list 1 2 3 4 5)))(newline)  ; -> 4 (1 5)
(display (greatest-diff (list 7 1 5 4)))(newline)    ; -> 4 (1 5)
(display (greatest-diff (list 9 4 3 2)))(newline)    ; -> -1 (none)
(display (greatest-diff (list 1 5 2 10)))(newline)   ; -> 9 (1 10)
