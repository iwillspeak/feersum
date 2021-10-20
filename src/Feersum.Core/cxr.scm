(define-library (scheme cxr)
    (import (scheme base))
    (export caar cadr cdar cddr)
    (begin
        (define (caar x) (car (car x)))
        (define (cadr x) (car (cdr x)))
        (define (cdar x) (cdr (car x)))
        (define (cddr x) (cdr (cdr x)))
        ))
