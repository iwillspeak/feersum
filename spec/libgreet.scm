(define-library (test greeter)
	(export greet)
	(import (scheme base))
	(begin
		(define (greet person)
			(display "Hello ")
			(display person)
			(display #\!)
			(newline))))

(import (prefix (only (test greeter) greet) grt/))

(grt/greet "world")