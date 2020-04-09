#lang eopl

;Exercise 1.36 [] Write a procedure g such that number-elements from page 23 could be defined as

; number-elements : ListOf(Symbol) -> ListOf(Pair)
(define number-elements
  (lambda (lst)
    (if (null? lst) '()
        (g (list 0 (car lst))
           (number-elements (cdr lst))))))


;g : ListOf(Pair) * ListOf(Pair) -> ListOf(Pair)
(define g
  (lambda (lst1 lst2)
    (cons lst1
          (map (lambda (ele) (list (+ (car ele) 1) (cadr ele))) lst2))))

; focus the type
        
        
