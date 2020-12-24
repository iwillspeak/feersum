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
        )
    ))(newline)