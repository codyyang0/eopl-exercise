#lang eopl

;Exercise 1.7 [**] The error message from nth-element is uninformative. Rewrite
;nth-element so that it produces a more informative error message, such as “(a b
;c) does not have 8 elements.”

; nth-element : List × Int → SchemeVal
; usage: (nth-element lst n) = the n-th element of lst

(define nth-element
  (lambda (lst n)
    (nth-element-i lst n lst n)))

; nth-element-i : List × Int × List × Int → SchemeVal
; lst save all list elements
; len save list's length
(define nth-element-i
  (lambda (saved-lst n lst len)
     (if (null? saved-lst)
         (report-list-too-short lst len)
         (if (zero? n)
             (car saved-lst)
             (nth-element-i (cdr saved-lst) (- n 1) lst len)))))

(define report-list-too-short
  (lambda (lst n)
    (eopl:error 'nth-element
                "~s does not have ~s elements.~%" lst (+ n 1))))


