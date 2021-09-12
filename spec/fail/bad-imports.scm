(import (not a real library))
(import badthing)
(define-library (test)
    (import (only (test greeter) greet)))