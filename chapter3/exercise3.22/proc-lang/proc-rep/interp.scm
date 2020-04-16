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
        (call-exp (op exps)
          (let ((vals (map (lambda (exp) (value-of exp env)) exps)))
            (let ((oper (value-of op env)))
              (if (procedure? oper) ; build-in operation
                  (begin
                    (let ((lov (map (lambda (v) (expval->num v)) vals)))
                      ; build-in operation procedure using build-in observer apply
                      (let ((res (apply oper lov)))
                        ; result's type judge, then contruct the related expval
                        (if (boolean? res)
                            (bool-val res)
                            (num-val res)))))
                  ; proc-val, not build-in operation
                  (let ((proc (expval->proc oper)))
                    (apply-procedure proc vals))))))

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
        
        (proc-exp (vars body)
          (proc-val (procedure vars body env)))

;        (call-exp (rator rands)
;          (let ((proc (expval->proc (value-of rator env)))
;                (args (map (lambda (rand) (value-of rand env)) rands)))
;            (apply-procedure proc args)))

        )))


  ;; procedure : Var * Exp * Env -> Proc
  ;; Page: 79
  (define procedure
    (lambda (vars body env)
      (lambda (vals)
        (value-of body (extend-env* vars vals env)))))
  
  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 79
  (define apply-procedure
    (lambda (proc vals)
      (proc vals)))

  )
