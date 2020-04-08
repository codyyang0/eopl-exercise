#lang eopl

; drop-until : Sym × Listof(Sym) → Listof(Sym)
(define drop-until
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            (cdr los)
            (drop-until s (cdr los))))))
