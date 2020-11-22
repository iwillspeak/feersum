(display
    (list
        (and (= 2 2) (> 2 1)) ; -> #t
        (and (= 2 2) (< 2 1)) ; -> #f
        (and 1 2 'c '(f g)) ; -> '(f g)
        (and) ; -> #t
    ))