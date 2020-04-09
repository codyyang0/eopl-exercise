#lang eopl

;Exercise 1.30 [**] (sort/predicate pred loi) returns a list of elements sorted by the predicate.

;sort : pred * ListOf(Int) -> ListOf(Int)
(define sort/predicate
  (lambda (pred loi)
    (sort-helper pred loi '())))

;sort-helper : pred * ListOf(Int) * ListOf(Int) -> ListOf(Int)
(define sort-helper
  (lambda (pred loi result)
    (if (null? loi)
        result
        (sort-helper pred (cdr loi) (sort-elem pred (car loi) result)))))

;sort-elem : pred * Int * ListOf(Int) -> ListOf(Int)
(define sort-elem
  (lambda (pred i result)
    (if (null? result)
        (list i)
        (if (pred i (car result))
            (cons i result)
            (cons (car result) (sort-elem pred i (cdr result)))))))