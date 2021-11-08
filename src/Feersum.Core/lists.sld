;;; Core Lits Library
;;
;; This library has the scheme portions of the low-level lists library
;; and is a sibling of the `(feersum serehfa lists)` library defined
;; in C#. Most consumers will not access it directly, but instead the
;; re-exported items from `(scheme base)`.
(define-library (feersum core lists)
  (import (feersum serehfa lists)
          (feersum serehfa arithmetics)
          (scheme write)
          (feersum builtin macros))
  (export append reverse list-tail)
  (begin
    ;;; Append Lists
    ;;
    ;; Given a list of lists this function recurses to append all elements
    ;; from each list into a new list.
    (define (append . lists)
      ;; Prepend the left list to the right, reversed. Used by
      ;; `append-one` to re-build the list after it is decomposed.
      (define (prepend-rev left right)
        (if (null? left)
            right
            (prepend-rev (cdr left) (cons (car left) right))))
      ;; Append a single pair of lists.
      (define (append-one left acc right)
        (if (null? left)
            (prepend-rev acc right)
            (append-one (cdr left) (cons (car left) acc) right)))
      ;; Recursively append the head of the list to the appended
      ;; tail. This function makes two simplifying assumptions:
      ;;
      ;; * The argument is a list of lists.
      ;; * The argument is not null.
      ;;
      ;; We can safely assume this based on how it is called below.
      (define (append-recurse lists)
        (if (null? (cdr lists))
          (car lists)
          (append-one
            (car lists)
            '()
            (append-recurse (cdr lists)))))
      ;; With the helpers defined appending a list of lists is
      ;; relatively simple. We have _two_ base cases here. One is
      ;; handled by the `append-recurse`. We handle the other here
      ;; directly.
      ;;
      ;; * No lists are passed to append, return the empty list.
      ;; * We are at the last list in the append call, just return it
      ;;   directly.
      (if (null? lists)
        '()
        (append-recurse lists)))
    
    ;;; Reverse a list
    ;;
    ;; A simple tail-recursive reverse implementaiton.
    (define (reverse list)
      (define (reverse-helper list acc)
        (if (null? list)
          acc
          (reverse-helper (cdr list) (cons (car list) acc))))
      (reverse-helper list '()))

    ;;; List Tail
    ;;
    ;; Walks the list and returns the k-th tail.
    (define (list-tail lst k)
      (if (zero? k)
        lst
        (list-tail (cdr lst) (- k 1))))
    ))
