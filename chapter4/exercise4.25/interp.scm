(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the IMPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  
  (provide result-of-program value-of instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; result-of-program : Program
  (define result-of-program 
    (lambda (pgm)
      (initialize-store!)
      (cases program pgm
        (a-program (st1)
          (result-of st1 (init-env))))))

  ;; result-of : statement
  (define result-of
    (lambda (st env)
      (cases statement st
        (assign-statement (var exp1)
          (setref! (apply-env env var) (value-of exp1 env)))
        (print-statement (exp1)
          (eopl:printf "~s~%" (value-of exp1 env)))
        (block-statement (sts)
          (for-each (lambda (st) (result-of st env)) sts))
        (if-statement (exp1 st1 st2)
          (if (expval->bool (value-of exp1 env))
              (result-of st1 env)
              (result-of st2 env)))
        (do-while-statement (st pred-exp)
          (begin
            (result-of st env)
            (when (expval->bool (value-of pred-exp env))
              (result-of (do-while-statement st pred-exp) env))))
        (while-statement (pred-exp st)
          (when (expval->bool (value-of pred-exp env))
            (result-of st env)
            (result-of (while-statement pred-exp st) env)))
        (var-statement (vars exps st)
          (letrec [[inner-extend-env
                    (lambda (vars exps env)
                      (if (null? vars)
                          env
                          (inner-extend-env (cdr vars) (cdr exps)
                                            (extend-env (car vars)
                                                        (newref (value-of (car exps) env))
                                                        env))))]]
            (result-of st (inner-extend-env vars exps env))))
        (read-statement (var)
          (let [[num (read)]]
            (if (and (number? num)
                     (not (negative? num)))
                (setref! (apply-env env var) (num-val num))
                (eopl:error 'value-of-statement "Expect a nonnegative integer, but got ~s." num))))
        
            )))
                                     

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 118, 119
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) 
        ;              = (deref (apply-env \r \x{}))}
        (var-exp (var) (deref (apply-env env var)))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))

        (add-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (+ num1 num2)))))

        (multi-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (* num1 num2)))))         

        (not-exp (exp1)
          (if (expval->bool (value-of exp1 env))
              (bool-val #f)
              (bool-val #t)))

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
          (let ((v1 (value-of exp1 env)))
            (value-of body
              (extend-env var (newref v1) env))))
        
        (proc-exp (vars body)
          (proc-val (procedure vars body env)))

        (call-exp (rator rands)
          (let [[proc (expval->proc (value-of rator env))]
                [args (map (lambda (rand) (value-of rand env)) rands)]]
            (apply-procedure proc args)))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env)))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of e1 env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps)))

        )))


  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 119

  ;; uninstrumented version
  ;;  (define apply-procedure
  ;;    (lambda (proc1 val)
  ;;      (cases proc proc1
  ;;        (procedure (var body saved-env)
  ;;          (value-of body
  ;;            (extend-env var (newref val) saved-env))))))
  
  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 args)
      (cases proc proc1
        (procedure (vars body saved-env)
          (let ((rs (newref* args)))
            (let ((new-env (extend-env* vars rs saved-env)))
              (when (instrument-let)
                (begin
                  (eopl:printf
                    "entering body of proc ~s with env =~%"
                    vars)
                  (pretty-print (env->list new-env)) 
                  (eopl:printf "store =~%")
                  (pretty-print (store->readable (get-store-as-list)))
                  (eopl:printf "~%")))
              (value-of body new-env)))))))  

  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (list
            (car p)
            (expval->printable (cadr p))))
        l)))

  )
  


  
