(import (scheme base)
    ;; FIXME: we shouldn't _need_ to import this, but we have no way of
    ;;        exporting or re-exporting macro definitions currently.
    (feersum builtin macros)
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
        
(display
    (cond
        (#f 'false)
        (#t 'true)
        (else 'fail)))
(display
    (cond
        (#f 'false)
        (#t 'true)))
(display
    (cond
        (else 'ok)))
(display
    (cond 
        (#f 'fail)))
