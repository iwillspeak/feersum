(import (scheme base)
    ;; FIXME: we shouldn't _need_ to import this, but we have no way of
    ;;        exporting or re-exporting macro definitions currently.
    (feersum builtin macros)
    (scheme write))

;; Without proper tail recursion this will overflow the stack:
;
; ! stack overflow
; [ping]  -> [ping] -> [ping] -> ... -> [ping]
; ....
;                   -> [ping] -> ... -> [ping]
;         -> [pong] -> [pong] -> ... -> [pong]
; [ping]  -> [ping] -> [ping] -> ... -> [ping]

;; With proper tail recursive calls stack use is constant: 
; [ping] -> [pong] -> [ping] -> [pong]

(display 
  (letrec ((ping (lambda (x)
  (pong x)))
         (pong (lambda (x)
          (if (> 0 x)
            x
            (ping (- x 1))))))
  (ping 100000)))


(newline)

; Tail context should flow into `and` and `or` macros
(display
  (letrec
    ((foo (lambda (x)
       (and #t
         (or #f
           (if (> 0 x)
             #t
             (foo (- x 1))))))))
   (foo 100000)))
