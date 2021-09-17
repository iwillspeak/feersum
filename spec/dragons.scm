(define-library (dragon)
    (import (scheme base)
    (scheme write))
    (export dragon)
    (begin

        (define (command cmd arg)
            (display "turtle.")
            (display cmd)
            (display "(")
            (display arg)
            (display ")")
            (newline))

        (define (init)
            (display "import turtle")(newline)
            (command "speed" 0)
            (command "tracer" "False")
            (command "showturtle" ""))

        (define (finalise)
            (command "done" ""))


        (define (dragon-iter iter size dir)
            (if (= 0 iter)
                (command "fd" size)
                (let ((next (- iter 1))
                      (size (* size 0.70711))
                      (turncmd (if (eq? dir 'left) "lt" "rt")))
                        (dragon-iter next size 'left)
                        (command turncmd 90)
                        (dragon-iter next size 'right))))

        (define (dragon iter)
            (init)
            (command "rt" (* 45 iter))
            (dragon-iter iter 450 'left)
            (finalise))
        
        ))

(import (dragon))

(dragon 3)