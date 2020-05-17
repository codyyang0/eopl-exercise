(module interp (lib "eopl.ss" "eopl")
  
  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  (require "pairval.scm")

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> Expval
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)
      (cases program pgm
        (a-program (body)
          (value-of body (init-env))))))


  ;; value-of : Exp * Env -> ExpVal
  (define value-of
    (lambda (exp env)
      (cases expression exp

        (const-exp (num) (num-val num))

        (var-exp (var) (deref (apply-env env var)))

        (diff-exp (exp1 exp2)
          (let ((val1
		  (expval->num
		    (value-of exp1 env)))
                (val2
		  (expval->num
		    (value-of exp2 env))))
            (num-val
	      (- val1 val2))))
        
        (zero?-exp (exp1)
	  (let ((val1 (expval->num (value-of exp1 env))))
	    (if (zero? val1)
	      (bool-val #t)
	      (bool-val #f))))

        (if-exp (exp0 exp1 exp2) 
          (if (expval->bool (value-of exp0 env))
            (value-of exp1 env)
            (value-of exp2 env)))

        (let-exp (vars exps body)       
          (let ((vals (map (lambda (exp) (value-of exp env)) exps)))
            (value-of body
              (extend-env* vars (newref* vals) env))))

        (proc-exp (bvars tys body)
	  (proc-val
	    (procedure bvars body env)))

        (call-exp (rator rands)          
          (let ((proc (expval->proc (value-of rator env)))
                (args (map (lambda (rand) (value-of rand env)) rands)))
	    (apply-procedure proc (newref* args))))

        (letrec-exp (ty1 p-name b-vars tys p-body letrec-body)
          (value-of letrec-body
            (extend-env-rec p-name b-vars p-body env)))

        (assign-exp (var exp)
          (let ([ref (apply-env env var)]
                [val (value-of exp env)])
            (setref! ref val)))

        (pair-exp (exp1 exp2)
          (let [[val1 (value-of exp1 env)]
                [val2 (value-of exp2 env)]]
            (pair-val (make-pair val1 val2))))

        (unpair-exp (var1 var2 exp body)
          (let [[p (expval->pair (value-of exp env))]]
            (let [[val1 (left p)]
                  [val2 (right p)]]
              (value-of body (extend-env var1 (newref val1)
                               (extend-env var2 (newref val2) env))))))
                                           

	    )))

  ;; apply-procedure : Proc * ExpVal -> ExpVal
  (define apply-procedure
    (lambda (proc1 args)
      (cases proc proc1
        (procedure (vars body saved-env)
          (value-of body (extend-env* vars args saved-env))))))
  
  )
  


  
