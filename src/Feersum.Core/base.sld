(define-library (scheme base)
    ;; Re-export Sherefa core items
    (import (feersum serehfa arithmetics)
        (feersum serehfa booleans)
        (feersum serehfa core)
        (feersum serehfa characters)
        (feersum serehfa equivalence)
        (feersum serehfa lists)
        (feersum serehfa strings)
        (feersum serehfa symbols)
        (feersum serehfa read)
        (feersum serehfa vectors)
        (feersum serehfa bytevectors))
    (begin
      ;;; List->vector conversion.
      ;;
      ;; Takes a scheme list and converts it into a sheme vector containing the
      ;; same elements in the same order. It is an error if the argument is not
      ;; a proper list. 
      (define (list->vector list)
        (define (fill-rec vec offset list)
          (if (null? list)
            vec
            (begin
              (vector-set! vec offset (car list))
              (fill-rec vec (+ 1 offset) (cdr list)))))
        (fill-rec (make-vector (length list)) 0 list))

      ;;; Apply a function to arguments
      ;;
      ;; Taks a list of arguments and applies the given function with them. If
      ;; more than one argument is specified then they are combined as if by
      ;; `(append (list arg...argn) args)`. That is the final element must be
      ;; a list, and any other arguments are prepended to that in order.
      (define (apply fun . args)
        (define (flatten args)
          (if (null? (cdr args))
            (car args)
            (cons (car args) (flatten (cdr args)))))
        (core-apply-vec fun (list->vector (flatten args)))))
    (export vector vector? vector-length vector-set! vector-ref make-vector 
        string=? string->number null? cons list make-list pair? list? car cdr
        set-car! set-cdr! length symbol? symbol=? symbol->string string->symbol
        eqv? eq? equal? bytevector? make-bytevector bytevector bytevector-length
        bytevector-u8-set! bytevector-u8-ref not boolean? boolean=? zero? + - /
        * = > < >= <= char? char=? char<? char>? char<=? char>=? char-ci=?
        char-ci<? char-ci>? char-ci<=? char-ci>=? char-alphabetic? char-numeric?
        char-whitespace? char-upper-case? char-lower-case? digit-value
        char->integer integer->char char-upcase char-downcase char-foldcase
        read-char peek-char read-line eof-object eof-object? char-ready?
        read-string apply list->vector procedure?)
    ;; Re-export base items from other definitions in this assembly
    (import (feersum core lists))
    (export append reverse list-tail list-ref list-set! list-copy memq memv member
      assv assq assoc)
    ;; Simple definitions
    (export caar cadr cdar cddr)
    (begin
        (define (caar x) (car (car x)))
        (define (cadr x) (car (cdr x)))
        (define (cdar x) (cdr (car x)))
        (define (cddr x) (cdr (cdr x)))))
