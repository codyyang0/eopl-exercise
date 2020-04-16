(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the PROC language, using the procedural
  ;; representation of procedures.

  ;; The \commentboxes are the latex code for inserting the rules into
  ;; the code in the book. These are too complicated to put here, see
  ;; the text, sorry. 

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))

        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))
        
        (proc-exp (var body)
          (let ((proc-env (free-env (proc-exp var body) env)))
            ;(eopl:printf "env : ~s~%" proc-env)
            (proc-val (procedure var body proc-env))))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg)))

        (traceproc-exp (var body)
          (begin
            (eopl:printf "Get in traceproce~%")
            (let ((val (value-of (proc-exp var body) env)))
              ; use negative effect : very useful
              (let ((nil (eopl:printf "Out of traceproce~%")))
                val))))
              
        )))


  ;; procedure : Var * Exp * Env -> Proc
  ;; Page: 79
  (define procedure
    (lambda (var body env)
      (lambda (val)
        (value-of body (extend-env var val env)))))
  
  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 79
  (define apply-procedure
    (lambda (proc val)
      (proc val)))


  ;;;;;;;;;;;;;;; free variables ;;;;;;;;;;;;;;;;;;;
  
  ;; occurs-free? : Exp * Var -> Boolean
  
  (define occurs-free?
    (lambda (exp search-var)
      (cases expression exp
        (const-exp (num) #f)
        
        (var-exp (var) (eqv? var search-var))
        
        (diff-exp (exp1 exp2)
          (or (occurs-free? exp1 search-var)
              (occurs-free? exp2 search-var)))

        (zero?-exp (exp1) (occurs-free? exp1 search-var))

        (if-exp (exp1 exp2 exp3)
          (or (occurs-free? exp1 search-var)
              (occurs-free? exp2 search-var)
              (occurs-free? exp3 search-var)))

        (let-exp (var exp1 body)
          (and (not (eqv? var search-var))
               (occurs-free? body search-var)))

        ; Look like let-exp expression, just the val of var is bound in the future, call-exp
        (proc-exp (var body)
          (and (not (eqv? var search-var))
               (occurs-free? body search-var)))

        (call-exp (rator rand)
          (or (occurs-free? rator search-var)
              (occurs-free? rand search-var)))

        (traceproc-exp (var body)
          (occurs-free? (proc-exp var body) search-var))
        )))
            
                  
  ;; free-env : Exp * Env -> Env
  (define free-env
    (lambda (exp env)
      (free-env-helper exp env (empty-env))))

  ;; free-env-helper : Exp * Env * Env -> Env
  (define free-env-helper
    (lambda (exp env res-env)
      (if (empty-env-record? env)
          res-env
          (let ((sym (extended-env-record->sym env))
                (val (extended-env-record->val env))
                (old-env (extended-env-record->old-env env)))
            (if (has-binding? res-env sym)
                (free-env-helper exp old-env res-env)
                (if (occurs-free? exp sym)
                    (free-env-helper exp old-env (extend-env sym val res-env))
                    (free-env-helper exp old-env res-env)))))))
  
  )
