(import (scheme base)
    (scheme write))

(define (mod a b)
  (if (< a b)
      a
      (mod (- a b) b)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (mod a b))))

(display (gcd 8 12))(newline)
(display (gcd 42 56))(newline)
(display (gcd 461952 116298))(newline)
(display (gcd 7966496 314080416))(newline)
(display (gcd 24826148 45296490))(newline)
(display (gcd 12 0))(newline)
(display (gcd 0 0))(newline)
(display (gcd 0 9))(newline)
