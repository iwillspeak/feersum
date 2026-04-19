(import (scheme base) (scheme write))

; A macro can generate define-syntax forms, creating new macro bindings
; as a side-effect of expansion.
(define-syntax jabberwocky
  (syntax-rules ()
    ((_ hatter)
     (begin
       (define march-hare 42)
       (define-syntax hatter
         (syntax-rules ()
           ((_) march-hare)))))))

(jabberwocky mad-hatter)
(display (mad-hatter))
(newline)
