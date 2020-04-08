#lang eopl

;Exercise 1.16 [*] (invert lst), where lst is a list of 2-lists (lists of length two),
;returns a list with each 2-list reversed.

; invert : lst → lst
(define invert
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (list (cadar lst) (caar lst))
              (invert (cdr lst))))))

; invert-map : lst → lst
(define invert-map
  (lambda (lst)
    (map (lambda (ele) (list (cadr ele) (car ele))) lst)))
  