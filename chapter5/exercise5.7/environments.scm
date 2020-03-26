(module environments (lib "eopl.ss" "eopl") 
  
  ;; builds environment interface, using data structures defined in
  ;; data-structures.scm. 

  (require "data-structures.scm")

  (provide init-env empty-env extend-env apply-env)

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

  ;; Page: 86
  (define apply-env
    (lambda (env search-sym)
      (cases environment env
        (empty-env ()
          (eopl:error 'apply-env "No binding for ~s" search-sym))
        (extend-env* (vars vals saved-env)
          (let ((find (memq search-sym vars)))
            (if (list? find)
                (let ((index (index-of search-sym vars)))
                  (list-ref vals index))
                (apply-env saved-env search-sym))))
        (extend-env (var val saved-env)
	  (if (eqv? search-sym var)
	    val
	    (apply-env saved-env search-sym)))
        (extend-env-rec (p-name b-var p-body saved-env)
          (if (eqv? search-sym p-name)
            (proc-val (procedure b-var p-body env))          
            (apply-env saved-env search-sym))))))

  ; index-of : identifier * listof(sym) -> int
  (define index-of
    (lambda (search-sym vars)
      (if (eqv? search-sym (car vars))
          0
          (+ 1 (index-of search-sym (cdr vars))))))
  )