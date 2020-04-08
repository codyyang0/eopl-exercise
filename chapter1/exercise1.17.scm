#lang eopl

;Exercise 1.17 [*] (down lst) wraps parentheses around each top-level element of
;lst.

; down : lst → lst
(define down
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (list (car lst))
              (down (cdr lst))))))

; down-map : lst → lst
(define down-map
  (lambda (lst)
    (map (lambda (ele) (list ele)) lst)))