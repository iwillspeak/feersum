; Mutually-recursive lambda test: two lambdas that reference each other
; This tests that Lower correctly handles capture chains where functions
; capture references to mutually-recursive peers

(import (scheme base))

(let ((even #f)
      (odd #f))
  (set! even (lambda (n)
               (if (= n 0)
                   #t
                   (odd (- n 1)))))
  (set! odd (lambda (n)
              (if (= n 0)
                  #f
                  (even (- n 1)))))
  (even 4))
