(import (scheme cxr)
    (scheme write))

(define test-list '(
    (1 2 3)
    (4 5 6)
    (7 8 9)
))

(display (caar test-list))(newline)
(display (cadr test-list))(newline)
(display (cdar test-list))(newline)
(display (cddr test-list))(newline)
