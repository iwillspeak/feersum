(import (scheme base)
  (scheme write))
(define (string-join list delimiter)
  (define (string-join-to result list)
    (if (null? list)
      result
      (string-join-to (string-append result delimiter (car list)) (cdr list))))
  (if (null? list)
    ""
    (string-join-to (car list) (cdr list))))
(display (string-append "<<{" (string-join '() "-") "}>>"))(newline)
(display (string-join (cons "foo" '()) "||"))(newline)
(display (string-join '("foo" "bar" "baz") ":"))(newline)
