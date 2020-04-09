#lang eopl

;Exercise 1.23 [**] (list-index pred lst) returns the 0-based position of the first element of lst that satisfies the predicate pred. If no element of lst satisfies the predicate, then list-index returns #f.

;list-index : pred * lst -> Int|#f
(define list-index
  (lambda (pred lst)
    (list-index-helper pred lst 0)))

(define list-index-helper
  (lambda (pred lst n)
    (if (null? lst)
        #f
        (if (pred (car lst))
            n
            (list-index-helper pred (cdr lst) (+ n 1))))))