#lang eopl

;Exercise 1.27 [**] (flatten slist) returns a list of the symbols contained in slist in the order in which they occur when slist is printed. Intuitively, flatten removes all the inner parentheses from its argument.

; slist ::= () | (sexp . slist)
; sexp ::= symbol | slist
;flatten : slist -> slist
(define flatten
  (lambda (slist)
    (if (null? slist)
        '()
        (append (flatten-sexp (car slist))
                (flatten (cdr slist))))))

;flatten-sexp : sexp -> list
(define flatten-sexp
  (lambda (sexp)
    (if (symbol? sexp)
        (list sexp)
        (flatten sexp))))
