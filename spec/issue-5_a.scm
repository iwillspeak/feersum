(import (scheme base)
    (scheme write))
((lambda (a)
    (display
        (if #f
              (lambda () a)
            a))
    a)
  101) ; -> should be 101, but is '()