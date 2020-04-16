(module environments (lib "eopl.ss" "eopl") 
  
  ;; builds environment interface, using data structures defined in
  ;; data-structures.scm. 

  (require "data-structures.scm")

  (provide init-env empty-env extend-env extend-env* apply-env)

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;
  
  ;; init-env : () -> Env
  ;; usage: (init-env) = [i=1, v=5, x=10]
  ;; (init-env) builds an environment in which i is bound to the
  ;; expressed value 1, v is bound to the expressed value 5, and x is
  ;; bound to the expressed value 10.
  ;; Page: 69
  (define init-env 
    (lambda ()
      (extend-env 
       'i (num-val 1)
       (extend-env
        'v (num-val 5)
        (extend-env
         'x (num-val 10)
         (empty-env))))))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

  (define empty-env
    (lambda ()
      (empty-env-record)))
  
  (define empty-env? 
    (lambda (x)
      (empty-env-record? x)))

  (define extend-env
    (lambda (sym val old-env)
      (extended-env-record sym val old-env)))

  (define extend-env*
    (lambda (syms vals old-env)
      (if (null? syms)
          old-env
          (extend-env* (cdr syms) (cdr vals)
                       (extend-env (car syms) (car vals) old-env)))))

  ; Exercise 3.22 [ ] The concrete syntax of this section uses different syntax
  ; for a built-in operation, such as difference, from a procedure call.
  (define apply-env
    (lambda (env search-sym)
      (if (empty-env? env)
          ; If can't find search in env, judge is it a built-in operation
          (let ((build-proc (eval search-sym)))
            (if (procedure? build-proc)
                build-proc
                (eopl:error 'apply-env "No binding for ~s" search-sym)))
          (let ((sym (extended-env-record->sym env))
                (val (extended-env-record->val env))
                (old-env (extended-env-record->old-env env)))
            (if (eqv? search-sym sym)
                val
                (apply-env old-env search-sym))))))
  )