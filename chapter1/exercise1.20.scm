#lang eopl

;Exercise 1.20 [*] (count-occurrences s slist) returns the number of occurrences of s in slist.

; what is Int, Int is 0 | (+ 1 Int)
; count-occurrences : sym * Slist -> Int
(define count-occurrences
  (lambda (s slist)
    (if (null? slist)
        0
        (+ (count-occurrences-exp s (car slist))
           (count-occurrences s (cdr slist))))))

; count-occurrences-exp : Sym * sExp -> Int
(define count-occurrences-exp
  (lambda (s sexp)
    (if (symbol? sexp)
        (if (eqv? sexp s) 1 0)
        (count-occurrences s sexp))))

; count-occurrences-inline : Sym * Slist -> Int
(define count-occurrences-inline
  (lambda (s slist)
    (if (null? slist)
        0
        (+
         (let ((sexp (car slist)))
           (if (symbol? sexp)
               (if (eqv? sexp s) 1 0)
               (count-occurrences-inline s sexp)))
         (count-occurrences-inline s (cdr slist))))))
         