(import (scheme base)
    (scheme write))
;; Implementation of the Sieve of Eratosthenes
;; https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
(define (eratosthenes n)
    ;; Mark multiples of the given prime in the vector
    (define (mark-multiples p marked)
        (define (mark-multiples-at p m marked)
            (if (>= m (vector-length marked))
                marked
                (begin
                    (vector-set! marked m #t)
                    (mark-multiples-at p (+ m p) marked))))
        (mark-multiples-at p (* p p) marked))
    ;; main prime sieve. Recursively marks multiples of each
    ;; prime and builds up a list of them as it goes.
    (define (siv p marked primes)
        (if (>= p (vector-length marked))
            primes
            ;; If the item is marked, it is a multiple of some other prime.
            (if (vector-ref marked p)
                (siv (+ 1 p) marked primes)
                (siv (+ 1 p) (mark-multiples p marked) (cons p primes)))))
    (siv 2 (make-vector n #f) ()))

;; ;; Display the first 10,000 primes
;; (display (eratosthenes 104730))
(display (eratosthenes 10000))
