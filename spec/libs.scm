(define-library (it should 100 support 202 numbers))
(define-library (test dependency))
(define-library (example grid)
	(export fiz buz
		(rename hidden-name external-name))
	(import (test dependency))
	(begin
		(define fiz 100)
		(define buz 100)
		(define hidden-name 100)
		(define not-exported 100)))

(define-library (test greeter)
	(export greet)
	; (import (scheme base))
	; (begin
	; 	(define (greet person)
	; 		(display "Hello ")
	; 		(display person)
	; 		(display #\!)
	; 		(newline)))
			)