(import (scheme base))
(=)  ; => #t
(> 5 4) ; => #t
(< 5 4) ; => #f
(if 
    (>= 1) ; => #t
    (+
        (+ 1 2 4 88) ; => 95
        (- 3 4 5) ; => -6
        (- 9) ; => -9
        (/) ; => 1
        (/ 2) ; => 0.5
        (/ 5 2) ; => 2.5
        (* 8 5) ; => 40
        (* 76) ; => 76
    ) ; => 200
    (* 8 9))