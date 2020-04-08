#lang eopl

; remove : Sym × Listof(Sym) → Listof(Sym)
; usage : removes all occurrences of a given symbol from a list of symbols,
(define remove
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            (remove s (cdr los))
            (cons (car los) (remove s (cdr los)))))))