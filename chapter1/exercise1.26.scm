#lang eopl

;Exercise 1.26 [**] (up lst) removes a pair of parentheses from each top-level element of lst. If a top-level element is not a list, it is included in the result, as is. The value of (up (down lst)) is equivalent to lst, but (down (up lst)) is not necessarily lst. (See exercise 1.17.)

;up : list -> list
(define up
  (lambda (lst)
    (if (null? lst)
        '()
        (if (list? (car lst))
            (append (car lst) (up (cdr lst)))
            (cons (car lst) (up (cdr lst)))))))
