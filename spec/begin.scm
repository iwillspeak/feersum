;; The `begin` special form takes a sequence and returns the value
;; of the last expression.
(begin
    12
    (begin 
        "hello"
        123
        "world")
    "hello world"
    (if (begin #f #t #f #t)
        (begin 1 2 3 45)
        (begin -2 -3 -9)))   # ~> 45