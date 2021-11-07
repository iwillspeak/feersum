﻿(define-library (scheme base)
    ;; Re-export Sherefa core items
    (import (feersum serehfa arithmetics)
        (feersum serehfa booleans)
        (feersum serehfa characters)
        (feersum serehfa equivalence)
        (feersum serehfa lists)
        (feersum serehfa strings)
        (feersum serehfa symbols)
        (feersum serehfa vectors)
        (feersum serehfa bytevectors))
    (export vector vector? vector-length vector-set! vector-ref make-vector 
        string=? string->number null? cons list make-list pair? list? car cdr
        set-car! set-cdr! length symbol? symbol=? symbol->string string->symbol
        eqv? eq? equal? bytevector? make-bytevector bytevector not boolean?
        boolean=? zero? + - / * = > < >= <= char? char=? char<? char>? char<=?
        char>=? char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
        char-alphabetic? char-numeric? char-whitespace? char-upper-case?
        char-lower-case? digit-value char->integer integer->char char-upcase
        char-downcase char-foldcase)
    ;; Re-export base items from other definitions in this assembly
    (import (feersum core lists))
    (export append reverse)
    ;; Simple definitions
    (export caar cadr cdar cddr)
    (begin
        (define (caar x) (car (car x)))
        (define (cadr x) (car (cdr x)))
        (define (cdar x) (cdr (car x)))
        (define (cddr x) (cdr (cdr x)))))