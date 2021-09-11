(define-library (test greeter)
	(export greet)
	(import (scheme base))
	(begin
		(define (greet person)
			(display "Hello ")
			(display person)
			(display #\!)
			(newline))))

(import (only (test greeter) greet))

(greet "world")