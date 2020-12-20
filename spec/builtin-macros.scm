(display
    (list
        (and (= 2 2) (> 2 1)) ; -> #t
        (and (= 2 2) (< 2 1)) ; -> #f
        (and 1 2 'c '(f g)) ; -> '(f g)
        (and) ; -> #t
        (or #t #t) ; -> #t
        (or #f #f #f #f #t #f #f) ; -> #t
        (or #f 100 #f 101) ; 100
        (or) ; -> #f
    ))