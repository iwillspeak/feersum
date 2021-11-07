(import (scheme base)
    ;; FIXME: we shouldn't _need_ to import this, but we have no way of
    ;;        exporting or re-exporting macro definitions currently.
    (feersum builtin macros)
    (scheme write))

(display (boolean=?
    #t
    
    (equal? #\alarm      #\x0007)  ; U+0007
    (equal? #\backspace  #\x0008)  ; U+0008
    (equal? #\delete     #\x007F)  ; U+007F
    (equal? #\escape     #\x001B)  ; U+001B
    (equal? #\newline    #\x000A)  ; the linefeed character, U+000A
    (equal? #\null       #\x0000)  ; the null character, U+0000
    (equal? #\return     #\x000D)  ; the return character, U+000D
    (equal? #\space      #\     )  ; the preferred way to write a space
    (equal? #\tab        #\x0009)  ; the tab character, U+0009
    (equal? #\x03BB      #\λ    )  ; λ
    ))(newline)

;; Simple character operations
(display (list
    (char? #\space)
    (char? #\tab)
    (char? #\null)
    (char? 101)
    (char? 00)
    
    (boolean=?
        #t
    
        (equal? (digit-value #\3)      3)
        (equal? (digit-value #\x0664)  4)
        (equal? (digit-value #\x0AE6)  0)
        (equal? (digit-value #\x0EA6) #f)
        (equal? (digit-value #\১)      1)
        (equal? (digit-value #\५)      5)
        )

    (boolean=?
        #t

        (equal? (char->integer #\space) 32)
        (equal? (char->integer #\null) 0)
        (equal? (char->integer #\x0EA6) 3750)
        (equal? (integer->char 38) #\&)
        (equal? (integer->char 0) #\x0000)
        (equal? (integer->char 55295) #\xD7FF)

        (let ((bad-char (integer->char 55296)))
            (and (> bad-char 1114111) (not (char? bad-char))))
        )
    ))(newline)

;; Character comparisons
(display (boolean=?
    #t
    
    (char=? #\a #\a #\a)
    (char-ci=? #\a #\a #\a)
    (char=? #\A #\A #\A)
    (char-ci=? #\A #\A #\A)
    (not (char=? #\a #\A #\a))
    (char-ci=? #\a #\A #\a)
    (char=? #\λ #\λ #\λ)
    (char-ci=? #\λ #\λ #\λ)
    (char=? #\Λ #\Λ #\Λ #\Λ #\Λ)
    (char-ci=? #\Λ #\Λ #\Λ #\Λ #\Λ)
    (not (char=? #\Λ #\λ #\Λ #\Λ #\Λ))
    (char-ci=? #\Λ #\λ #\Λ #\Λ #\Λ)
    (not (char=? #\λ #\λ #\λ #\space))
    (not (char-ci=? #\λ #\λ #\λ #\space))
    (not (char=? #\a #\λ))
    (not (char-ci=? #\a #\λ))

    (char<? #\a #\b #\c #\d)
    (char-ci<? #\a #\b #\c #\d)
    (not (char<? #\A #\b #\c #\D))
    (char-ci<? #\A #\b #\c #\D)
    (not (char<? #\d #\c #\b #\a))
    (not (char-ci<? #\d #\c #\b #\a))
    (not (char<? #\D #\c #\b #\A))
    (not (char-ci<? #\D #\c #\b #\A))

    (char<=? #\a #\b #\c #\d)
    (char-ci<=? #\a #\b #\c #\d)
    (not (char<=? #\A #\b #\c #\D))
    (char-ci<=? #\A #\b #\c #\D)
    (not (char<=? #\d #\c #\b #\a))
    (not (char-ci<=? #\d #\c #\b #\a))
    (not (char<=? #\D #\c #\b #\A))
    (not (char-ci<=? #\D #\c #\b #\A))
    (char<=? #\λ #\λ #\λ)
    (char-ci<=? #\λ #\λ #\λ)
    (char<=? #\Λ #\Λ #\Λ #\Λ #\Λ)
    (char-ci<=? #\Λ #\Λ #\Λ #\Λ #\Λ)
    (not (char<=? #\Λ #\λ #\Λ #\Λ #\Λ))
    (char-ci<=? #\Λ #\λ #\Λ #\Λ #\Λ)

    (not (char>? #\a #\b #\c #\d))
    (not (char-ci>? #\a #\b #\c #\d))
    (not (char>? #\A #\b #\c #\D))
    (not (char-ci>? #\A #\b #\c #\D))
    (char>? #\d #\c #\b #\a)
    (char-ci>? #\d #\c #\b #\a)
    (not (char>? #\D #\c #\b #\A))
    (char-ci>? #\D #\c #\b #\A)

    (not (char>=? #\a #\b #\c #\d))
    (not (char-ci>=? #\a #\b #\c #\d))
    (not (char>=? #\A #\b #\c #\D))
    (not (char-ci>=? #\A #\b #\c #\D))
    (char>=? #\d #\c #\b #\a)
    (char-ci>=? #\d #\c #\b #\a)
    (not (char>=? #\D #\c #\b #\A))
    (char-ci>=? #\D #\c #\b #\A)
    (char>=? #\λ #\λ #\λ)
    (char-ci>=? #\λ #\λ #\λ)
    (char>=? #\Λ #\Λ #\Λ #\Λ #\Λ)
    (char-ci>=? #\Λ #\Λ #\Λ #\Λ #\Λ)
    (not (char>=? #\Λ #\λ #\Λ #\Λ #\Λ))
    (char-ci>=? #\Λ #\λ #\Λ #\Λ #\Λ)

    ))(newline)

;; Character class predicates
(display (boolean=?
    #t

    (char-alphabetic? #\a)
    (not (char-alphabetic? #\५))
    (char-alphabetic? #\λ)
    (not (char-alphabetic? #\null))
    (not (char-alphabetic? #\space))

    (not (char-numeric? #\a))
    (char-numeric? #\५)
    (not (char-numeric? #\λ))
    (not (char-numeric? #\null))
    (not (char-numeric? #\space))

    (not (char-whitespace? #\a))
    (not (char-whitespace? #\५))
    (not (char-whitespace? #\λ))
    (not (char-whitespace? #\null))
    (char-whitespace? #\space)
    (char-whitespace? #\tab)
    
    (not (char-upper-case? #\a))
    (char-upper-case? #\A)
    (not (char-upper-case? #\५))
    (not (char-upper-case? #\λ))
    (char-upper-case? #\Λ)
    (not (char-upper-case? #\null))
    (not (char-upper-case? #\space))
    (not (char-upper-case? #\tab))

    (char-lower-case? #\a)
    (not (char-lower-case? #\A))
    (not (char-lower-case? #\५))
    (char-lower-case? #\λ)
    (not (char-lower-case? #\Λ))
    (not (char-lower-case? #\null))
    (not (char-lower-case? #\space))
    (not (char-lower-case? #\tab))

    ))

;; casing transforms
(display (list
    (char-upcase #\a)
    (char-upcase #\A)
    (char-upcase #\५)
    (char-upcase #\λ)
    (char-upcase #\null)
    (char-upcase #\space)
    (char-downcase #\a)
    (char-downcase #\A)
    (char-downcase #\५)
    (char-downcase #\Λ)
    (char-upcase #\null)
    (char-upcase #\space)
    (char-foldcase #\a)
    (char-foldcase #\A)
    (char-foldcase #\५)
    (char-foldcase #\Λ)
    (char-upcase #\null)
    (char-upcase #\space)
    ))
