(import (scheme base)
    (scheme write))
(display (list
    (when #t 1 2 3)
    (when #f 'fail)
    (unless #f 'yarp)
    (unless #true 123)
    ))

(display
    (if #t
        255
        -123))

;; Hello isn't #f, therefore we should return "world"
(display
    (if "hello"
        "world"
        -2))
        
(define-syntax my-cond
    (syntax-rules (else)
        ((cond (else e ...))   (begin e ...))
        ((cond (test e e1 ...))
            (if test
                (begin e e1 ...)))
        ((cond (test e e1 ...) c ...)
            (if test
                (begin e e1 ...)
                (my-cond c ...)))))
(display
    (my-cond
        (#f 'false)
        (#t 'true)
        (else 'fail)))
(display
    (my-cond
        (#f 'false)
        (#t 'true)))
(display
    (my-cond
        (else 'ok)))
(display
    (my-cond 
        (#f 'fail)))
