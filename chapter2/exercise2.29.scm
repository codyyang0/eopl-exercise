#lang eopl

;Exercise 2.29 [*] Where a Kleene star or plus (page 7) is used in concrete syntax, it
;is most convenient to use a list of associated subtrees when constructing an abstract
;syntax tree.

; identifier? : symbol -> Bool
(define identifier?
  (lambda (s)
    (and (symbol? s)
         (not (eqv? 'lambda s)))))
    

(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-vars (list-of identifier?))
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rands (list-of lc-exp?))))

;parse-expression : schemeVal -> LcExp
(define parse-expression
  (lambda (datum)
    (cond
      ((identifier? datum) (var-exp datum))
      ((pair? datum)
       (if (eqv? (car datum) 'lambda)
           (lambda-exp (cadr datum) (parse-expression (caddr datum)))
           (if (> (length datum) 1)
               (app-exp (parse-expression (car datum))
                        (map parse-expression (cadr datum)))
               (map parse-expression datum))))
      (else
       (report-invalid-concrete-syntax datum)))))

(define report-invalid-concrete-syntax
  (lambda (datum)
    (eopl:error 'parse-expression "Syntax error: ~s" datum)))

; test example
(parse-expression '(lambda (x) (x (y))))