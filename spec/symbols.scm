(import (scheme base))
(display (list
    (symbol? ’foo) ; -> #t
    (symbol? (car ’(a b))) ; -> #t
    (symbol? "bar") ; -> #f
    (symbol? ’nil) ; -> #t
    (symbol? ’()) ; -> #f
    (symbol? #f) ; -> #f
    ))(newline)
(display (list
    (symbol=? 'hello 'hello 'hello)
    (symbol=? "nope" 'nope)
    ))(newline)
(display (list
    (symbol->string ’flying-fish) ; -> "flying-fish"
    (symbol->string ’Martin) ; -> "Martin"
    (symbol->string (string->symbol "Malvina")) ; -> "Malvina"
    (string->symbol "mISSISSIppi") ; -> mISSISSIppi
    (eqv? ’bitBlt (string->symbol "bitBlt")) ; -> #t
    (eqv? ’LollyPop(string->symbol(symbol->string ’LollyPop))) ; -> #t
    (string=? "K. Harper, M.D."(symbol->string(string->symbol "K. Harper, M.D."))) ; -> #t
    ))(newline)