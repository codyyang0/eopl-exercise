(module environments (lib "eopl.ss" "eopl") 
  
  (require "data-structures.scm")

  (provide init-env empty-env extend-env apply-env)

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;
  
  ;; init-env : () -> environment

  ;; (init-env) builds an environment in which i is bound to the
  ;; expressed value 1, v is bound to the expressed value 5, and x is
  ;; bound to the expressed value 10.  

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

  (define apply-env
    (lambda (env search-sym)
      (cases environment env
        (empty-env ()
          (eopl:error 'apply-env "No binding for ~s" search-sym))
        (extend-env (bvar bval saved-env)
	  (if (eqv? search-sym bvar)
	    bval
	    (apply-env saved-env search-sym)))
        (extend-env* (bvars bvals saved-env)
          (let [[n (location bvars search-sym)]]
            (if n
                (list-ref bvals n)
                (apply-env saved-env search-sym))))
        (extend-env-rec (p-name b-vars p-body saved-env)
          (if (eqv? search-sym p-name)
             (proc-val (procedure b-vars p-body env))          
             (apply-env saved-env search-sym))))))

    (define location
      (lambda (syms search-sym)
        (cond
          [(null? syms) #f]
          [(eqv? search-sym (car syms)) 0]
          [(location (cdr syms) search-sym)
           => (lambda (n)
                (+ n 1))]
          [else #f])))
    
  )