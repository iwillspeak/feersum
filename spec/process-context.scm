(import (scheme process-context)
  (scheme base)
  (scheme write))

(display (get-environment-variable "FEERSUM_TESTING"))
(display (assoc "FEERSUM_TESTING" (get-environment-variables)))

(exit 123)
