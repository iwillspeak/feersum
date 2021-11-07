(import (scheme base)
    ;; FIXME: we shouldn't _need_ to import this, but we have no way of
    ;;        exporting or re-exporting macro definitions currently.
    (feersum builtin macros)
    (scheme write))
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
