(define-library (it should 100 support 202 numbers))
(define-library (test dependency))
(define-library (example grid)
	(export fiz buz
		(rename hidden-name external-name))
	(import (test dependency))
	(begin
		(define fiz 100)
		(define buz 100)
		(define hidden-name 101)
		(define not-exported 100)))
(define-library (test greeter)
	(import (prefix (example grid) grid/))
	(export greet)
	(begin (define greet grid/external-name)))
(import (only (test greeter) greet))
(display greet)