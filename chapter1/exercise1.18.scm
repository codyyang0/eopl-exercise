#lang eopl

;Exercise 1.18 [*] (swapper s1 s2 slist) returns a list the same as slist, but
;with all occurrences of s1 replaced by s2 and all occurrences of s2 replaced by s1.

; swapper : sym1 * Sym2 * Slist -> Slist
(define swapper
  (lambda (s1 s2 slist)
    (if (null? slist)
        '()
        (cons (swapper-s-exp s1 s2 (car slist))
              (swapper s1 s2 (cdr slist))))))

; swapper-s-exp : sym1 * Sym2 * sExp -> sym | slist
(define swapper-s-exp
  (lambda (s1 s2 sexp)
    (if (symbol? sexp)
        (cond ((eqv? sexp s1) s2)
              ((eqv? sexp s2) s1)
              (else sexp))
        (swapper s1 s2 sexp))))

;(swapper 'a 'd '(a b c d))
;(swapper 'a 'd '(a d () c d))
;(swapper 'x 'y '((x) y (z (x))))

; swapper-inline : sym1 * Sym2 * Slist -> Slist
(define swapper-inline
  (lambda (s1 s2 slist)
    (if (null? slist)
        '()
        (cons
         (let ((sexp (car slist)))
           (if (symbol? sexp)
               (cond ((eqv? sexp s1) s2)
                     ((eqv? sexp s2) s1)
                     (else sexp))
               (swapper-inline s1 s2 sexp)))
         (swapper-inline s1 s2 (cdr slist))))))

; swapper-map : sym1 * Sym2 * Slist -> Slist
(define swapper-map
  (lambda (s1 s2 slist)
    (map (swpper-sexp-map s1 s2) slist)))

; swapper-sexp-map : Sym1 * Sym2 -> (sym) -> sym | slist
(define swpper-sexp-map
  (lambda (s1 s2)
    (lambda (sexp)
      (if (symbol? sexp)
          (cond ((eqv? sexp s1) s2)
                ((eqv? sexp s2) s1)
                (else sexp))
          (swapper-map s1 s2 sexp)))))
         