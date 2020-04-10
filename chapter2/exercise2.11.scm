#lang eopl

;exercise 2.11, ribcage representation

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
    (cons (cons (list var) (list val)) env)))

; constructor
; extend-env* : Vars * SchemeVals * Env -> Env
(define extend-env*
  (lambda (vars vals env)
    (cons (cons vars vals) env)))

;look like object-orient extends List operations
;index : symbol * ListOf(symbol) -> Int | #f
(define index
  (lambda (s los)
    (index-helper s los 0)))

;index-helper : symbol * ListOf(Symbol) * Int -> Int | #f
(define index-helper
  (lambda (s los n)
    (cond
      ((null? los) #f)
      ((eqv? s (car los)) n)
      (else
       (index-helper s (cdr los) (+ 1 n))))))


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
       (let ((vars (caar env))
             (vals (cdar env))
             (saved-env (cdr env)))
         (let ((ref (index search-var vars)))
           (if ref
               (list-ref vals ref)
               (apply-env-helper saved-env search-var all-env))))))))

; has-binding?-helper : Env * Var * Env -> boolean
(define has-binding?-helper
  (lambda (env search-var all-env)
    (cond
      ((empty-env? env) #f)
      ((not (pair? (car env)))
       (report-invalid-env all-env))
      (else
       (let ((vars (caar env))
             (saved-env (cdr env)))
         (or (number? (index search-var vars))
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
