#lang eopl

;Exercise 1.13 [**] In our example, we began by eliminating the Kleene star in the
;grammar for S-list. Write subst following the original grammar by using map.

(define subst
  (lambda (new old slist)
    (map (subst-in-s-exp new old) slist)))

(define subst-in-s-exp
  (lambda (new old)
    (lambda (sexp)
      (if (symbol? sexp)
          (if (eqv? sexp old) new sexp)
          (subst new old sexp)))))

; use map