(import (scheme write)
  (scheme base))

(define-syntax pp
  (syntax-rules ()
    ((_ x ...) (begin (display 'x ...)(display " -> ")(display x ...)(newline)))))

(define-syntax my-macro
  (syntax-rules ()
    ((_ a b . c)
      (list 'a-was: a 'b-was: b 'c-was: 'c)))) ; The 'c here should quote the _subst_

(pp (my-macro 1 2 3 4))
;; => (a-was: 1 b-was: 2 c-was: (3 4))

(pp (my-macro 1 2 3))
;; => (a-was: 1 b-was: 2 c-was: (3))

(pp (my-macro 1 2))
;; => (a-was: 1 b-was: 2 c-was: ())

(pp (my-macro 1 2 . 3))   ; improper list literal — also matches!
;; => (a-was: 1 b-was: 2 c-was: 3)

(define-syntax my-vec-macro
  (syntax-rules ()
    ((_ #(a b c)) (list '(a b) c))
    ((_ #(a ...)) (list a ...))))

(pp (my-vec-macro #(1 2 3)))  ; => ((1 2) 3)
(pp (my-vec-macro #(1 2 3 4 5)))  ; => (1 2 3 4 5)
(pp (my-vec-macro #(1 2)))  ; => (1 2)

(define-syntax my-greedy-macro
  (syntax-rules ()
    ((_ a b ... c) (list 'a-got: a 'b-got: '(b ...) 'c-got: 'c))))

(pp (my-greedy-macro 1 2 3))         ;; => (a-got: 1 b-got: (2) c-got: 3)
(pp (my-greedy-macro 1 2 3 4 5))     ;; => (a-got: 1 b-got: (2 3 4) c-got: 5)
(pp (my-greedy-macro 1 (2 3) (4 5))) ;; => (a-got: 1 b-got: ((2 3)) c-got: (4 5))
(pp (my-greedy-macro 1 2))           ;; => (a-got: 1 b-got: () c-got: 2)

(define-syntax my-repeating-macro
  (syntax-rules ()
    ((_ #(a ...) #(b ...)) (list '(a . b) ...))))

(pp (my-repeating-macro #(1 2 3) #(4 5 6))) ;; => ((1 . 4) (2 . 5) (3 . 6))
