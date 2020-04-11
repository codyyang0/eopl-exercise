#lang eopl

;Exercise 2.28 [*] Write an unparser that converts the abstract syntax of an lc-exp
;into a string that matches the second grammar in this section (page 52).


;pred
(define identifier?
  (lambda (s)
    (and (symbol? s)
         (not (eqv? 'lambda s)))))

; constructor
(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-var identifier?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))


;unparse-lc-exp : LcExp -> String
(define unparse-lc-exp
  (lambda (exp)
    (cases lc-exp exp
      (var-exp (var) (symbol->string var))
      (lambda-exp (bound-var body)
        (string-append "(" "lambda" " (" (symbol->string bound-var) ") "
                       (unparse-lc-exp body) ")"))
      (app-exp (rator rand)
        (string-append "(" (unparse-lc-exp rator) " " (unparse-lc-exp rand) ")")))))

; test example
(define e (lambda-exp 'x
             (app-exp (var-exp 'f)
                      (app-exp (var-exp 'f)
                               (var-exp 'x)))))

(unparse-lc-exp e)
