# Compiler Error Index

Each diagnostic emitted from the compiler has a unique error number of the form
`SCMXXX` where `XXX` is the error number. Error numbers are grouped into
'categories' based on the phase in the compiler that emitted the diagnostic.

Currently the diagnostic categories are:

 * `1-9` - Legacy syntax errors.
 * `10-19` - Parser and read.
 * `20-29` - Library declaration and use.
 * `30-39` - Binder diagnostics.
 * `40-49` - Macro definition and expansion.

## Parser Errors (10-19)

### SCM010 - Parse error

Generic syntax parsing error. This is raised when the parser encounters syntax that cannot be parsed correctly and no more specific error is available.

Possible causes:
- Unexpected tokens or token sequences
- Malformed expressions
- Unterminated strings or identifiers
- Invalid syntax constructs

Example:
```scheme
;; Invalid syntax with unclosed parentheses
(define-library .
```

Or with malformed string or identifier literals:
```scheme
;; Invalid hex character code in string
"\x;"

;; Unclosed identifier literal
'|\x
```

## Library Errors (20-29)

### SCM020 - Improper library name

The library name is valid but does not follow best practices or uses a reserved namespace. This is a warning, not an error.

Possible causes:
- Using reserved namespace prefixes like "scheme" or "srfi"
- Library naming conventions not followed

Example:
```scheme
;; Using reserved 'scheme' prefix
(define-library (scheme custom))

;; Using reserved 'srfi' prefix
(define-library (srfi trust me))
```

### SCM021 - Invalid library name

The library name provided is not valid according to the Scheme library naming rules.

Possible causes:
- Invalid characters in library name components
- Improperly formatted library name
- Expected a library name but found something else

Example:
```scheme
;; String instead of a list
(define-library "hello")

;; Invalid characters in name component
(define-library (scheme |silly'chars|))

;; Invalid name component (not a symbol)
(define-library (some 't))
```

### SCM022 - Malformed library declaration

A library declaration is syntactically invalid or incorrectly structured.

Possible causes:
- Missing required components in library declaration
- Incorrect form of library declaration

Example:
```scheme
;; Missing library name
(define-library)

;; Improper declaration structure
(define-library (my-lib)
  export) ;; Missing parentheses around export
```

## Binder Errors (30-39)

### SCM030 - Invalid or incomplete pattern syntax

A pattern does not follow the expected syntax rules.

Possible causes:
- Invalid pattern structure in a binding position
- Incomplete pattern specification

Example:
```scheme
;; Invalid match pattern
(match x
  ((1 . )) ;; Invalid pattern, missing value after dot
  (#t 1))
```

### SCM031 - Missing export

Warning that occurs when a library attempts to export a symbol that isn't defined within the library.

Possible causes:
- Typo in export name
- Referencing a non-existent definition
- Export declared before the corresponding definition

Example:
```scheme
(define-library (missing export)
    ;; Attempting to export symbols that don't exist
    (export we |don't| exist))
```

### SCM032 - Ill-formed special form

A special form (like `if`, `let`, `set!`, etc.) is not syntactically correct.

Possible causes:
- Wrong number of arguments to a special form
- Invalid syntax for a special form
- Malformed special form components

Example:
```scheme
(define n 1)
(lambda ()
    ;; Malformed set! with closing parenthesis on the next line
    (set! n (+ 1 n) 
    n))
```

### SCM033 - Invalid formal parameter pattern

A formal parameter pattern in a lambda expression or procedure definition is invalid.

Possible causes:
- Invalid structure in formal parameters list
- Duplicate parameter names
- Improper use of rest parameters

Example:
```scheme
;; Invalid lambda parameters (duplicate parameter name)
(lambda (x y x) 
  (+ x y))

;; Invalid placement of dotted parameter
(lambda (. rest)
  rest)
```

### SCM034 - Invalid let binding

A binding in a `let` expression is not correctly formed.

Possible causes:
- Binding not in the form of (name value)
- Invalid binding structure

Example:
```scheme
;; Missing value in let binding
(let ((x)) 
  x)

;; Malformed binding (not a proper list)
(let ((x 1 2)) 
  x)
```

### SCM035 - Reference to undefined symbol

A symbol is referenced that hasn't been defined in the current context.

Possible causes:
- Typo in variable name
- Using a variable before it's defined
- Referencing a variable outside of its scope
- Missing import for a library symbol

Example:
```scheme
;; Using a variable that hasn't been defined
(let-syntax
    ((another (syntax-rules ()
        ((_ a ...) '(a ...)))))
    (another 1 2 3))

;; Attempting to use nonexistent symbol
(display unknown-variable)
```

### SCM036 - Invalid import declaration

An import declaration is invalid or references a non-existent library.

Possible causes:
- Library doesn't exist
- Invalid import set syntax
- Malformed import specifier

Example:
```scheme
;; Importing a non-existent library
(import (not a real library))

;; Invalid import structure
(import badthing)

;; Referencing non-existent library in import 
(define-library (test)
    (import (only (test greeter) greet)))
```

### SCM037 - Use of uninitialised variable

A variable is used before it has been initialized, particularly in a `letrec` binding.

Possible causes:
- Referencing a variable in a `letrec` binding before all variables are bound
- Cyclic dependencies between variables in mutually recursive definitions

Example:
```scheme
(letrec ((first (+ second 1))  ;; Error: second isn't initialized yet
         (second first))       ;; Error: first's initialization uses second
  second)

(letrec* ((first (- second 100))  ;; Error: second isn't initialized yet
          (second first))         ;; This is fine in letrec*
  second)
```

### SCM038 - Invalid datum value

A datum literal value is invalid or malformed.

Possible causes:
- Invalid format for a numeric literal
- Malformed vector or list literal
- Other invalid literal expressions

Example:
```scheme
;; Invalid character code
#\x12345

;; Invalid numeric literals
#e1.2.3

;; Malformed vector
#(1 2 . 3)
```

## Macro Errors (40-49)

### SCM040 - Macro expansion error

An error occurred while expanding a macro.

Possible causes:
- No pattern in the macro matched the provided syntax
- Invalid use of a macro
- Syntax error in macro template or pattern
- Invalid dotted form in macro pattern
- Invalid use of ellipses in macro patterns

Example:
```scheme
(define-syntax test-macro
    (syntax-rules ::: (test)
        ((_) 'nowt)
        ((_ test things :::) (list things :::))))

;; Error: No pattern matches this use of the macro
(test-macro fail this one)

;; Error: Invalid dotted form in macro pattern
(let-syntax
    ((another (syntax-rules ()
        ((. _) (. "test")))))  ;; Invalid dotted form
    (another 1 2 3))
```
