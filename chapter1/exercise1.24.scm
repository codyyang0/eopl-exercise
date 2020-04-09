#lang eopl

;Exercise 1.24 [**] (every? pred lst) returns #f if any element of lst fails to satisfy pred, and returns #t otherwise.

;every? : pred * lst -> boolean
(define every?
  (lambda (pred lst)
    (if (null? lst)
        #t
        (and (pred (car lst))
             (every? pred (cdr lst))))))
