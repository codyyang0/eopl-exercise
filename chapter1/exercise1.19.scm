#lang eopl

;Exercise 1.19 [**] (list-set lst n x) returns a list like lst, except that the n-th
;element, using zero-based indexing, is x.

;list-set : lst * Int * schemeVal -> lst
(define list-set
  (lambda (lst n x)
    (if (null? lst)
        (report-list-too-short n)
        (if (= n 0)
            (cons x (cdr lst))
            (cons (car lst) (list-set (cdr lst) (- n 1) x))))))

(define report-list-too-short
  (lambda (n)
    (eopl:error 'list-set
                "List too short by ~s elements.~%" (+ n 1))))