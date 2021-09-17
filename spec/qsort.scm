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

        ;; Append the `right` list to the `left` one.
        ;
        ; This function uses two helpers to tail-recursively reconstruct a list.
        ; The `append-helper` first recurses moving elements from the `left`
        ; list into the accumuulator. Once the left list is empty the base case
        ; uses `rebuild` to tail-recursively construct the final list.
        (define (append left right)
            (define (rebuild acc right)
                (if (null? acc)
                    right
                    (rebuild (cdr acc) (cons (car acc) right))))
            (define (append-helper acc left right)
                (if (null? left)
                    (rebuild acc right)
                    (append-helper (cons (car left) acc)
                        (cdr left)
                        right)))
            (append-helper '() left right))

        ;; Recursive quicksort implementation.
        ;
        ; Takes a list of numbers and returns a sorted list. Trivially sorted
        ; lists are immediately returned. Complex lists are split using a token
        ; pivot and the high and low sub-lists are individually sorted
        ; recurisvely.
        (define (qsort nums)
            (if (or (null? nums) (null? (cdr nums)))
                nums
                (let* ((pivot (car nums))
                        (partitioned (partition pivot (cdr nums))))
                    (append (qsort (car partitioned))
                        (cons pivot (qsort (cdr partitioned)))))))
        ))

(import
    (scheme base)
    (scheme write)
    (sort))

(display (qsort '(79 23 45 6 9)))(newline)
(display (qsort '(9 8 7 6 5 4 3 2 1 0 0 1 2 3 4 5 6 7 8 9)))(newline)