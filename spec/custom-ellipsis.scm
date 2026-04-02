(import (scheme base) (scheme write))

; syntax-rules allows a custom ellipsis identifier as the second element.
; Here `dots` replaces the standard `...` for pattern repetition.
(define-syntax my-seq
  (syntax-rules dots ()
    ((my-seq expr dots)
     (begin expr dots))))

; (my-seq 1 2 3) expands to (begin 1 2 3) => 3
(display (my-seq 1 2 3))
(newline)

; Single element
(display (my-seq 99))
(newline)
