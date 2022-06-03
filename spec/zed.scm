(import (scheme base)
  (scheme write))

;; This is the zed combinator from lambda calculus.
(define (Z f)
  ((lambda (g) (g g))
   (lambda (g)
    (f (lambda a (apply (g g) a))))))

;; Echo just prints out its arguments
(define echo
  (Z (lambda (echo)
    (lambda args
      (display args)))))

(echo "hello world")(newline)

;; Recursive fibonacci defined using the combinator.
(define fib
  (Z (lambda (fib)
    (lambda (n)
      (if (<= n 2)
        1
        (+ (fib (- n 1)) (fib (- n 2))))))))

(display (fib 5))(newline)

;; Recursive factorial defined using the combinator.
(define fact
  (Z (lambda (fact)
    (lambda (n)
      (if (= n 0)
        1
        (* n (fact (- n 1))))))))

(display (fact 12))(newline)
