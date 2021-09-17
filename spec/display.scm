(import (scheme base)
    (scheme write))
(display "")(newline) ; => 
(display ())(newline) ; =>
(display 123)(newline) ; => 123
(display "Hello World")(display #\!)(newline) ; => Hello World!
(display '("I \" need \v some \x00; escaping"))(newline) ; => 