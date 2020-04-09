#lang eopl

;Exercise 1.22 [**] (filter-in pred lst) returns the list of those elements in lst that satisfy the predicate pred.

; list = () | schemeVal . list
;filter-in : pred * list -> list
(define filter-in
  (lambda (pred lst)
    (if (null? lst)
        '()
        (if (pred (car lst))
            (cons (car lst) (filter-in pred (cdr lst)))
            (filter-in pred (cdr lst))))))


;filter-in : pred * list -> list
;usage: filter all element in the list, include nest element of list
(define filter-in-all
  (lambda (pred lst)
    (if (null? lst)
        '()
        (if (list? (car lst))
            (append (filter-in pred (car lst))
                    (filter-in pred (cdr lst)))
            (if (pred (car lst))
                (cons (car lst) (filter-in pred (cdr lst)))
                (filter-in pred (cdr lst)))))))


