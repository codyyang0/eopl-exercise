(module environments (lib "eopl.ss" "eopl") 
  
  ;; builds environment interface, using data structures defined in
  ;; data-structures.scm. 

  (require "data-structures.scm")
  (require "store.scm")

  (provide init-env! extend-env! extend-env*! extend-env-rec! get-env apply-env)

  (define env 'uninitialized)

  ;; look like a single instance factory?
  
;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;
  
  ;; init-env : () -> Env
  ;; usage: (init-env) = [i=1, v=5, x=10]
  ;; (init-env) builds an environment in which i is bound to the
  ;; expressed value 1, v is bound to the expressed value 5, and x is
  ;; bound to the expressed value 10.
  ;; Page: 69

  ; init-env! : () -> nil
  (define init-env!
    (lambda ()
      (begin
        (set! env (empty-env))
        (extend-env! 'i (newref (num-val 1)))
        (extend-env! 'v (newref (num-val 5)))
        (extend-env! 'x (newref (num-val 10))))))
    
  ;;;;;;;;;;;;;; extend-env!, extend-env*!, extend-env-rec! ;;;;;;;;;;;;

  ; extend-env! : Var * Ref -> nil
  (define extend-env!
    (lambda (var ref)
      (set! env (extend-env var ref env))))

  ; extend-env*! : Vars * Refs -> nil
  (define extend-env*!
    (lambda (bvars vrefs)
      (set! env (extend-env* bvars vrefs env))))

  ; extend-env-rec! : Identifier * Var * Exp -> nil
  (define extend-env-rec!
    (lambda (p-name bvar p-body)
      (set! env (extend-env-rec p-name bvar p-body env))))

  ; get-env : () -> Env
  (define get-env
    (lambda () env))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

  ;; Page: 86
  ; apply-env : Env -> Reference
  (define apply-env
    (lambda (search-sym)
      (cases environment env
        (empty-env ()
          (eopl:error 'apply-env "No binding for ~s" search-sym))
        (extend-env* (vars refs saved-env)
          (let ((find (memq search-sym vars)))
            (if (list? find)
                (let ((index (index-of search-sym vars)))
                  (list-ref refs index))
                (apply-saved-env saved-env search-sym))))
        (extend-env (var ref saved-env)
	  (if (eqv? search-sym var)
	    ref
            (apply-saved-env saved-env search-sym)))
        (extend-env-rec (p-name b-var p-body saved-env)
          (if (eqv? search-sym p-name)
            (newref (proc-val (procedure b-var p-body env)))
            (apply-saved-env saved-env search-sym))))))

  ; apply-saved-env
  (define apply-saved-env
    (lambda (saved-env search-sym) 
      (let ((cur-env env))
        (set! env saved-env)
        (let ((ref (apply-env search-sym)))
          (set! env cur-env)
          ref))))

  ; index-of : identifier * listof(sym) -> int
  (define index-of
    (lambda (search-sym vars)
      (if (eqv? search-sym (car vars))
          0
          (+ 1 (index-of search-sym (cdr vars))))))
  )