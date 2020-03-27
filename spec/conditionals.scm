(if #t
    255
    -123)

;; Hello isn't #f, therefore we should return "world"
(if "hello"
    "world"
    -2)