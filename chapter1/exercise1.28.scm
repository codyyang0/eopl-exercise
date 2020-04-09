#lang eopl

;Exercise 1.28 [**] (merge loi1 loi2), where loi1 and loi2 are lists of integers that are sorted in ascending order, returns a sorted list of all the integers in loi1 and loi2.

;merge : ListOf(Int) * ListOf(Int) -> ListOf(Int)
(define merge
  (lambda (loi1 loi2)
    (merge-helper loi1 loi2 '())))

; look like Collection Sort , Spark merge operations
;merge-helper : ListOf(Int) * ListOf(Int) * ListOf(Int) -> ListOf(Int)
(define merge-helper
  (lambda (loi1 loi2 result)
    (cond
      ((null? loi1) (append result loi2))
      ((null? loi2) (append result loi1))
      (else
       (let ((i1 (car loi1))
             (i2 (car loi2)))
         (if (< i1 i2)
             (merge-helper (cdr loi1) loi2 (append result (list i1)))
             (merge-helper loi1 (cdr loi2) (append result (list i2)))))))))
