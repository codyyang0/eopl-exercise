#lang eopl

;Exercise 2.22 [*] Using define-datatype, implement the stack data type of exer- cise 2.4.

; constructor
(define-datatype stack stack?
  (empty-stack)
  (push
   (ele (lambda (e) #t))
   (saved-stack stack?)))

; observer
(define pop
  (lambda (st)
    (cases stack st
      (empty-stack () (report-error-pop))
      (push (ele saved-stack) saved-stack))))

; observer
(define top
  (lambda (st)
    (cases stack st
      (empty-stack () (empty-stack))
      (push (ele saved-stack) ele))))

; observer | predicate
(define empty-stack?
  (lambda (st)
    (cases stack st
      (empty-stack () #t)
      (else #f))))

(define report-error-pop
  (lambda ()
    (eopl:error 'pop
                "Can't pop from an empty stack")))