(import (scheme base)
    (scheme write))

(define (swap me you)
    (let ((me you)
        (you me))
        you))

(let ((x 1) (y 2))
    (define-syntax swap!
        (syntax-rules ()
            ((swap! a b)
             (let ((tmp a))
                (set! a b)
                (set! b tmp)))))
    (swap! x y)
    (display (list x y)))

(display (swap "Fooble" "Barble"))

(= (swap 123 "Test") 123)