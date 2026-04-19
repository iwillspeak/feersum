; Hygiene tests for user-defined macros.
;
; The classic `my-or` uses a `temp` binding in its expansion template.
; Hygienic expansion must ensure that `temp` introduced by the macro never
; captures a `temp` visible at the call site.
(import (scheme base) (scheme write))

(define-syntax my-or
    (syntax-rules ()
        ((_) #f)
        ((_ e) e)
        ((_ e1 e2 ...)
            (let ((temp e1))
                (if temp temp (my-or e2 ...))))))

; ── Basic functionality ────────────────────────────────────────────────────────

(display (my-or))             (newline) ; => #f            (zero args)
(display (my-or #f))          (newline) ; => #f            (single false)
(display (my-or 42))          (newline) ; => 42            (single truthy)
(display (my-or #f 42))       (newline) ; => 42            (first false, second truthy)
(display (my-or 1 2 3))       (newline) ; => 1             (short-circuits on first truthy)
(display (my-or #f #f 99))    (newline) ; => 99            (all false until last)
(display (my-or #f #f #f #f)) (newline) ; => #f            (all false)

; ── Hygiene proof ─────────────────────────────────────────────────────────────
;
; If expansion were naive (non-hygienic), `(my-or #f temp)` would expand to:
;
;   (let ((temp #f))
;     (if temp temp (my-or temp)))
;
; ...where the final `temp` would refer to the let-bound #f rather than the user's
; `temp`, yielding #f.  A hygienic system renames the macro-introduced `temp` so
; the user's `temp` is unaffected, yielding #t.

(define temp #t)
(display (my-or #f temp))     (newline) ; => #t  (HYGIENE: user's temp is not captured)
