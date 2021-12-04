(define-library (scheme base)
    ;; Re-export Sherefa core items
    (import (feersum serehfa arithmetics)
        (feersum serehfa booleans)
        (feersum serehfa characters)
        (feersum serehfa equivalence)
        (feersum serehfa lists)
        (feersum serehfa strings)
        (feersum serehfa symbols)
        (feersum serehfa read)
        (feersum serehfa vectors)
        (feersum serehfa bytevectors))
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
        read-string)
    ;; Re-export base items from other definitions in this assembly
    (import (feersum core lists))
    (export append reverse list-tail list-ref list-set! list-copy memq memv member
      assv assq assoc)
    ;; Simple definitions
    (export caar cadr cdar cddr)
    (begin
        (define fizz hello-world)

        (define (caar x) (car (car x)))
        (define (cadr x) (car (cdr x)))
        (define (cdar x) (cdr (car x)))
        (define (cddr x) (cdr (cdr x)))))
