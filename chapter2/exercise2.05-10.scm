#lang eopl

;exercise 2.5, 2.7, 2.8, 2.9, 2.10
;Exercise 2.5 [*] We can use any data structure for representing environments, if we can distinguish empty environments from non-empty ones, and in which one can extract the pieces of a non-empty environment. Implement environments using a representation in which the empty environment is represented as the empty list, and in which extend-env builds an environment that looks like

; every datatype is from atom to complex, recursive datatype
; operation should be the datatype supply.
; Env = (empty-env) | (extend-env Var SchemeVal Env)
; Var = Sym

; constructor
; empty-env : () -> Env
(define empty-env
  (lambda () '()))

; observer
; empty-env? : env -> boolean
(define empty-env? null?)

; constructor
; extend-env : Var * SchemeVal * Env -> Env
(define extend-env
  (lambda (var val env)
    (cons (list var val) env)))

; constructor
; extend-env* : Vars * SchemeVals * Env -> Env
(define extend-env*
  (lambda (vars vals env)
    (if (null? vars)
        env
        (let ((var (car vars))
              (val (car vals))
              (rest-vars (cdr vars))
              (rest-vals (cdr vals)))
          (extend-env* rest-vars rest-vals
                       (extend-env var val env))))))

; observer
; apply-env : Env * Var -> SchemeVal
(define apply-env
  (lambda (env search-var)
    (apply-env-helper env search-var env)))

; observer
; apply-env : Env * Var * Env -> SchemeVal
(define apply-env-helper
  (lambda (env search-var all-env)
    (cond
      ((empty-env? env)
        (report-no-binding-found search-var all-env))
      ((not (pair? (car env)))
       (report-invalid-env all-env))
      (else
       (let ((var (caar env))
             (val (cadar env))
             (saved-env (cdr env)))
         (if (eqv? var search-var)
             val
             (apply-env-helper saved-env search-var all-env)))))))

; has-binding?-helper : Env * Var * Env -> boolean
(define has-binding?-helper
  (lambda (env search-var all-env)
    (cond
      ((empty-env? env) #f)
      ((not (pair? (car env)))
       (report-invalid-env all-env))
      (else
       (let ((var (caar env))
             (saved-env (cdr env)))
         (or (eqv? search-var var)
             (has-binding?-helper saved-env search-var all-env)))))))

; has-binding? : Env * Var -> boolean
(define has-binding?
  (lambda (env search-var)
    (has-binding?-helper env search-var env)))

(define report-no-binding-found
  (lambda (search-var env)
    (eopl:error 'apply-env "No binding for ~s in env: ~s~%" search-var env)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))

; test
(define e
  (extend-env 'd 6
    (extend-env 'y 8
     (extend-env 'x 7
       (extend-env 'y 14
         (empty-env))))))