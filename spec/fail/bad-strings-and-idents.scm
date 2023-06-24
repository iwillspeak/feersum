;; FIXME: Almost none of these generate parser errors any more. We should be
;;        more strict about what we accept as strings or idents.

"\x;"
"\x20"
"\x10000;\"
'|"
#\x12345
'|\x;|
'|\x
