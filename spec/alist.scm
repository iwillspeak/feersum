(import (scheme write)
  (scheme base))
(define-syntax alist
  (syntax-rules ()
     ((_) ())
     ((_ x y z ...)
      (cons (cons x y) (alist z ...)))))
(display (alist 'foo 10 'bar 20 'baz 30))(newline)
