; Plain scope: each variant of malformed dotted form

; ( . a) - no head before dot
(. "no head")
; (.) - no head and no tail
(.)
; (a .) - no tail after dot
(a .)

; In define-syntax: malformed dotted forms in patterns and templates
(define-syntax bad-macro
    (syntax-rules ()
        ; (. _) invalid pattern: no head before dot
        ((. _) #f)
        ; (.) invalid template: no head and no tail
        ((_) (.))
        ; (b .) invalid template: no tail after dot
        ((_ b) (b .))))

; In define-library: malformed dotted forms in library body
(define-library (fail dotted library)
    (begin
        ; ( . a) in library
        (. "library")
        ; (.) in library
        (.)
        ; (c .) in library
        (c .)))
