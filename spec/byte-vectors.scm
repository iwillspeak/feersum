(import (scheme base)
    (scheme write))
(display (list 

    ;; byte vector literals
    #u8()
    #u8(255)
    #u8( 1 2 3 )

    (bytevector? #u8())
    (bytevector? #u8(128 254 255))

    ;; byte vector constructor
    (bytevector)
    (bytevector 1 2 4 5)
    (make-bytevector 10 90)

    ))(newline)

(define test-vector #u8(1 2 3 4 5))
(display (bytevector-length test-vector))(newline)
(display (bytevector-u8-ref test-vector 3))(newline)
(bytevector-u8-set! test-vector 3 101)
(display (bytevector-u8-ref test-vector 3))(newline)
