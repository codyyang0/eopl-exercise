#lang eopl


; Env = (empty-env) | (extend-env Var SchemeVal Env)
; Var = Sym

; empty-env : () -> Env
(define empty-env
  (lambda () '()))

; empty-env? : env -> boolean
(define empty-env? null?)

; extend-env : Var * SchemeVal * Env -> Env
(define extend-env
  (lambda (var val env)
    (cons (list var val) env)))

; apply-env : Env * Var -> SchemeVal
(define apply-env
  (lambda (env search-var)
    (cond
      ((empty-env? env)
        (report-no-binding-found search-var))
      ((not (pair? (car env)))
       (report-invalid-env env))
      (else
       (let ((var (caar env))
             (val (cadar env))
             (saved-env (cdr env)))
         (if (eqv? var search-var)
             val
             (apply-env saved-env search-var)))))))


(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

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