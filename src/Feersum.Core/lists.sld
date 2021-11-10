;;; Core Lits Library
;;
;; This library has the scheme portions of the low-level lists library
;; and is a sibling of the `(feersum serehfa lists)` library defined
;; in C#. Most consumers will not access it directly, but instead the
;; re-exported items from `(scheme base)`.
(define-library (feersum core lists)
  (import (feersum serehfa lists)
          (feersum serehfa arithmetics)
          (feersum serehfa equivalence)
          (scheme write)
          (feersum builtin macros))
  (export append reverse list-tail list-ref list-set! list-copy memq memv member)
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

    ;;; List Ref
    ;;
    ;; Returns the `k`th item from the list.
    (define (list-ref lst k)
      (car (list-tail lst k)))

    ;;; List Set Index
    ;;
    ;; Mutates the `k`th element in the list to become `val`.
    (define (list-set! lst k val)
      (set-car! (list-tail lst k) val))

    ;;; List Copy
    ;;
    ;; If `lst` is a proper list then a new list is built contianing the same
    ;; elements in `car` position, but with new pairs.
    (define (list-copy lst)
      (if (pair? lst)
        (cons (car lst) (list-copy (cdr lst)))
        lst))

    ;;; List Member Search with Eq
    ;;
    ;; Returns the tail of the list starting from the first element that
    ;; compares equal to `obj` using `eq?`.
    (define (memq obj lst)
      (if (null? lst)
        #f
        (if (eq? (car lst) obj)
          lst
          (memq obj (cdr lst)))))

    ;;; List Member Search with Eqv
    ;;
    ;; Returns the tail of the list starting from the first element that
    ;; compares equal to the `obj` using `eqv?`.
    (define (memv obj lst)
      (if (null? lst)
        #f
        (if (eqv? (car lst) obj)
          lst
          (memv obj (cdr lst)))))

    ;;; List Member Search with comparator
    ;;
    ;; Returns the tail of the list starting from the first element that
    ;; compares equal to the `obj` using the givne `cmp`arator, or `equal?` if
    ;; no comparator is provided.
    (define (member . params)
      (define (member-cmp obj lst cmp)
        (if (null? lst)
          #f
          (if (cmp (car lst) obj)
            lst
            (member-cmp obj (cdr lst) cmp))))
      ;; FIXME: re-write once case-lambda is supported.
      (let ((obj (car params))
            (lst (list-ref params 1))
            (cmp (list-tail params 2)))
        (if (null? cmp)
          (member-cmp obj lst equal?)
          (member-cmp obj lst (car cmp)))))
    ))
