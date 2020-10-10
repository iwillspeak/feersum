(define (swap me you)
    (let ((me you)
        (you me))
        you))

(display (swap "Fooble" "Barble"))

(= (swap 123 "Test") 123)