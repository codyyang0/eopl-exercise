#lang eopl

;Exercise 2.21 [*] Implement the data type of environments, as in section 2.2.2, using
;define-datatype. Then include has-binding? of exercise 2.9.

; constructor
(define-datatype environment environment?
  (empty-env)
  (extend-env
   (var symbol?)
   (val (lambda (val) #t))
   (saved-env environment?)))

; observer
; apply-env : env * var -> schemeVal
(define apply-env
  (lambda (env search-var)
    (cases environment env
      (empty-env () (report-no-binding-found search-var))
      (extend-env (var val saved-env)
        (if (eqv? var search-var)
            val
            (apply-env saved-env search-var))))))

; predicate
; has-binding? : env * var -> schemeVal
(define has-binding?
  (lambda (env search-var)
    (cases environment env
      (empty-env () #f)
      (extend-env (var val saved-env)
        (if (eqv? var search-var) #t (has-binding? saved-env search-var))))))



(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

;(define report-invalid-env
;  (lambda (env)
;    (eopl:error 'apply-env "Bad environment: ~s" env)))