(import (scheme base))
(display (list
    
    ;; vector new
    (vector 1 2 3)
    (make-vector 10 #\a)

    ;; Vector literals
    #(0 (2 2 2 2) "Anna")

    ;; Vector type check
    (vector? #())
    (vector? #(1 2 3))
    (vector? "Test")

    ;; General vector method
    (vector-length #(#\a #\b #\c))
    (vector-ref #(a #f "test") 2)
    (vector-ref '#(1 1 2 3 5 8 13 21) 5)
    (let ((vec (vector 0 '(2 2 2 2) "Anna")))
        (vector-set! vec 1 '("Sue" "Sue"))
        vec)

    ))