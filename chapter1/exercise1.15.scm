#lang eopl
;Exercise 1.15 [*] (duple n x) returns a list containing n copies of x.

; duple : Int × SchemeVal → ListOf(SchemeVal)
(define duple
  (lambda (n x)
    (if (= n 0)
        '()
        (cons x (duple (- n 1) x)))))
