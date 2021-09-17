(import (scheme base)
    (scheme write))
(display (list
    (when #t 1 2 3)
    (when #f 'fail)
    (unless #f 'yarp)
    (unless #true 123)
    ))