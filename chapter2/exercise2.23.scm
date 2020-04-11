#lang eopl

;Exercise 2.23 [*] The definition of lc-exp ignores the condition in definition 1.1.8 that
;says “Identifier is any symbol other than lambda.” Modify the definition of identifier?
;to capture this condition. As a hint, remember that any predicate can be used in
;define-datatype, even ones you define.

;pred
(define identifier?
  (lambda (s)
    (and (symbol? s)
         (not (eqv? 'lambda s)))))

; constructor
(define-datatype LcExp LcExp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-var identifier?)
   (body LcExp?))
  (app-exp
   (rator LcExp?)
   (rand LcExp?)))
