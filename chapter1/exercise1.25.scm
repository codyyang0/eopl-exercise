#lang eopl

;Exercise1.25 [**](exists? pred lst) returns #t if any element of lst satisfies pred, and returns #f otherwise.

;exists? : pred * lst -> boolean
(define exists?
  (lambda (pred lst)
    (if (null? lst)
        #f
        (or (pred (car lst))
            (exists? pred (cdr lst))))))
