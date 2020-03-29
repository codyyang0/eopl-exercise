(module interp (lib "eopl.ss" "eopl")
  
  ;; cps interpreter for the LETREC language, using the data structure
  ;; representation of continuations (Figure 5.3).

  ;; exercise: rewrite this using the procedural representation of
  ;; continuations (Figure 5.2).

  ;; exercise: rewrite this using a trampoline (page 159).

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")

  ; (provide value-of-program value-of/k )
  (provide (all-defined-out))

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> FinalAnswer
  ;; Page: 143 and 154
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)
      (cases program pgm
        (a-program (exp1)
          (value-of/k exp1 (init-env) (end-cont))))))  

  ;; value-of/k : Exp * Env * Cont -> FinalAnswer
  ;; Page: 143--146, and 154
  (define value-of/k
    (lambda (exp env cont)
      (cases expression exp
        (const-exp (num) (apply-cont cont (num-val num)))
        
        (var-exp (var) (apply-cont cont (deref (apply-env env var))))
        
        (diff-exp (exp1 exp2)
          (value-of/k exp1 env
            (diff1-cont exp2 env cont)))
        
        (zero?-exp (exp1)
          (value-of/k exp1 env
            (zero1-cont cont)))
        
        (if-exp (exp1 exp2 exp3)
          (value-of/k exp1 env
            (if-test-cont exp2 exp3 env cont)))
        
        (let-exp (var exp body)
          (value-of/k exp env (let-exp-cont var body env cont)))
        
        (proc-exp (var body)
          (apply-cont cont 
            (proc-val (procedure var body env))))
        
        (call-exp (rator rand) 
          (value-of/k rator env
            (rator-cont rand env cont)))
        
        (letrec-exp (p-name b-var p-body letrec-body)
          (value-of/k letrec-body
            (extend-env-rec p-name b-var p-body env)
            cont))

        ; Exercise 5.9
        (assign-exp (var exp)
          (value-of/k exp env (set-rhs-cont env var cont)))

        (begin-exp (exp1 exps)
          (begin
            (value-of/k exp1 env (begin-exp-cont exps env cont))))
        
   )))

  ;; apply-procedure/k : Proc * ExpVal * Cont -> FinalAnswer
  ;; Page 152 and 155
  (define apply-procedure/k
    (lambda (proc1 arg cont)
      (cases proc proc1
        (procedure (var body saved-env)
          (value-of/k body
            (extend-env var (newref arg) saved-env)
            cont)))))

  ;; Cont = Expval -> FinalAnswer
  (define continuation?
    (lambda (cont)
      (procedure? cont)))

  ; end-cont : () -> Cont
  (define end-cont
    (lambda ()
      (lambda (val)
        (begin
          (eopl:printf "End of computation.~%")
          val))))

  ; zero1-cont : Cont -> Cont
  (define zero1-cont
    (lambda (cont)
      (lambda (val)
        (apply-cont cont
                    (bool-val (zero? (expval->num val)))))))
  
  ; let-exp-cont : Var * Exp * Env * Cont -> Cont
  (define let-exp-cont
    (lambda (var body env cont)
      (lambda (val)
        (value-of/k body (extend-env var (newref val) env) cont))))

  ;if-test-cont : Exp * Exp * Env * Cont -> Cont
  (define if-test-cont
    (lambda (exp2 exp3 env cont)
      (lambda (val)
        (if (expval->bool val)
            (value-of/k exp2 env cont)
            (value-of/k exp3 env cont)))))

  ; apply-cont : Cont * ExpVal -> FinalAnswer
  (define apply-cont
    (lambda (cont v)
      (cont v)))

  (define diff1-cont
    (lambda (exp2 env cont)
      (lambda (val1)
        (value-of/k exp2 env (diff2-cont val1 env cont)))))

  (define diff2-cont
    (lambda (val1 env cont)
      (lambda (val2)
        (apply-cont cont
                    (num-val (- (expval->num val1)
                                (expval->num val2)))))))
  (define rator-cont
    (lambda (rand env cont)
      (lambda (val)
        (value-of/k rand env (rand-cont val cont)))))

  (define rand-cont
    (lambda (rator cont)
      (lambda (val)
        (apply-procedure/k (expval->proc rator) val cont))))

  ; set-rhs-cont : Env * Var * Cont -> Cont
  (define set-rhs-cont
    (lambda (env var cont)
      (lambda (val)
        (begin
          (setref!
           (apply-env env var)
           val)
          (apply-cont cont (num-val 27))))))

  ; begin-exp-cont : Exps * Env * Cont -> Cont
  (define begin-exp-cont
    (lambda (exps env cont)
      (lambda (val)
        (begin
          (if (null? exps)
              (apply-cont cont val)
              (value-of/k (car exps) env (begin-exp-cont (cdr exps) env cont)))))))

  )
  


  
