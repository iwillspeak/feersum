(import (scheme base))

(letrec ((first (+ second 1)) ; Can't reference second here, it isn't intialised
    (second first))           ; Can't reference first here, initialisation
                              ; order isn't specified.
    second)
(letrec* ((first (- second 100)) ; Can't referenc second, not initialised
    (second first))              ; This is actually OK.
    second)
