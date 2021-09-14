;; Sorting library example
(define-library (sort)
    (import (scheme base))
    (export qsort)
    (begin
        ;; Partition Utitlity
        ;
        ; This function takes a given `lst` and partitions it around a `pivot`.
        ; The result is a list who's `car` is a list containing the numbers less
        ; than the pivot, and `cdr` is a list containing those greater than the
        ; pivot.
        (define (partition pivot lst)
            (if (null? lst)
                '(())
                (let ((current (car lst))
                    (rest (partition pivot (cdr lst))))
                    (if (< current pivot)
                        (cons (cons current (car rest))
                            (cdr rest))
                        (cons (car rest)
                            (cons current (cdr rest)))))))

        ;; Append the `rigth` list to the `left` one.
        (define (append left right)
            (if (null? left)
                right
                (cons (car left)
                    (append (cdr left) right))))

        ;; Recursive quicksort implementation.
        ;
        ; Takes a list of numbers and returns a sorted list. Trivially sorted
        ; lists are immediately returned. Complex lists are split using a token
        ; pivot and the high and low sub-lists are individually sorted
        ; recurisvely.
        (define (qsort nums)
            (if (or (null? nums) (null? (cdr nums)))
                nums
                (letrec ((pivot (car nums))
                        (partitioned (partition pivot (cdr nums))))
                    (append (qsort (car partitioned))
                        (cons pivot (qsort (cdr partitioned)))))))
        ))

(import (sort))

(display (qsort '(79 23 45 6 9)))(newline)
(display (qsort '(9 8 7 6 5 4 3 2 1 0 0 1 2 3 4 5 6 7 8 9)))(newline)