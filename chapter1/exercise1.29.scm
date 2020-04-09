#lang eopl

;Exercise 1.29 [**] (sort loi) returns a list of the elements of loi in ascending order.

;sort : ListOf(Int) -> ListOf(Int)
(define sort
  (lambda (loi)
    (sort-helper loi '())))

;sort-helper : ListOf(Int) * ListOf(Int) -> ListOf(Int)
(define sort-helper
  (lambda (loi result)
    (if (null? loi)
        result
        (sort-helper (cdr loi) (sort-elem (car loi) result)))))

;sort-elem : Int * ListOf(Int) -> ListOf(Int)
(define sort-elem
  (lambda (i result)
    (if (null? result)
        (list i)
        (if (< i (car result))
            (cons i result)
            (cons (car result) (sort-elem i (cdr result)))))))