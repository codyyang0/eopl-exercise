#lang eopl

;Exercise 2.13 [**] Extend the procedural representation to implement empty-env? by representing the environment by a list of two procedures: one that returns the value associated with a variable, as before, and one that returns whether or not the environment is empty.

; env : ListOf(var -> schemeVal; () -> boolean)

; empty-env : () -> env
(define empty-env
  (lambda ()
    (list (lambda () #t)
          (lambda (search-var)
            (report-no-binding-found search-var)))))

; extend-env : Var * SchemeVal * Env -> Env
(define extend-env
  (lambda (saved-var saved-val saved-env)
    (list (lambda () #f)
          (lambda (search-var)
            (if (eqv? search-var saved-var) saved-val
                (apply-env saved-env search-var))))))

; empty-env? : env -> boolean
(define empty-env?
  (lambda (env)
    ((apply env 0))))

; apply-env : Env * Var -> SchemeVal
(define apply-env
  (lambda (env search-var)
    ((apply env 1) search-var)))

; apply get the procedure in the env
; apply : Env * Int -> procedure
(define apply
  (lambda (env n)
    (if (list? env)
        (list-ref env n)
        (report-invalid-env env))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))
    
