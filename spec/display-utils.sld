(define-library (example displayutils)
	(export display-all)
	(import (scheme base)
		(scheme write))
	(begin
		(define (display-all things)
			(if (null? things)
				(newline)
				(begin
					(display (car things))
					(display-all (cdr things)))))))