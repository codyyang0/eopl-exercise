#lang eopl

;Exercise 1.21 [**] (product sos1 sos2), where sos1 and sos2 are each a list of symbols without repetitions, returns a list of 2-lists that represents the Cartesian product of sos1 and sos2. The 2-lists may appear in any order.

; product : ListOf(Sym) * ListOf(Sym) -> ListOf(Pair(Sym))
(define product
  (lambda (sos1 sos2)
    (product-aux sos1 sos2 '())))


; product-aux : ListOf(Sym) * ListOf(Sym) * ListOf(Pair(Sym)) -> ListOf(Pair(Sym))
(define product-aux
  (lambda (sos1 sos2 result)
    (if (null? sos1)
        result
        (product-aux (cdr sos1) sos2 (append (join (car sos1) sos2) result)))))


; join : Sym * ListOf(Sym) -> ListOf(Pair)
(define join
  (lambda (s sos)
    (if (null? sos)
        '()
        (cons (list s (car sos))
              (join s (cdr sos))))))