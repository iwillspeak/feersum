(define-library (multifile test)
    (export greet)
    (import (scheme write))
    (begin
        (define (greet thing)
            (display "Greetings from another file '")(display thing)(display "'!\n"))))
