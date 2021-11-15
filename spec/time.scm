(import (scheme base)
  (scheme write)
  (scheme time))

(define (time-length x)
  (let ((list (make-list x))
        (start (current-jiffy)))
    (length list)
    (/ (- (current-jiffy) start)
       (jiffies-per-second))))

(time-length 10)
(time-length 1000)

0
