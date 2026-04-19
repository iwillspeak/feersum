;; Bad strings and identifiers. All of these should generate reader-level
;; diagnostics rather than silently producing wrong output.

"\x;"
"\x20"
"\x10000;\"
'|"
#\x12345
'|\x;|
'|\x
