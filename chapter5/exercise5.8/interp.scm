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

  (provide value-of-program value-of/k)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> FinalAnswer
  ;; Page: 143 and 154
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of/k exp1 (init-env) (end-cont))))))  

  ;; value-of/k : Exp * Env * Cont -> FinalAnswer
  ;; Page: 143--146, and 154
  (define value-of/k
    (lambda (exp env cont)
      (cases expression exp
        (const-exp (num) (apply-cont cont (num-val num)))
        
        (var-exp (var) (apply-cont cont (apply-env env var)))

        ; Exercise 5.8
        ; Only apply the saved cont to proc-val
        (proc-exp (vars body)
          (apply-cont cont 
            (proc-val (procedure vars body env))))
        
        (let-exp (var exp1 body)
          (value-of/k exp1 env
            (let-exp-cont var body env cont)))

        (diff-exp (exp1 exp2)
          (value-of/k exp1 env
            (diff1-cont exp2 env cont)))

        (zero?-exp (exp1)
          (value-of/k exp1 env
            (zero1-cont cont)))

        (if-exp (exp1 exp2 exp3)
          (value-of/k exp1 env
            (if-test-cont exp2 exp3 env cont)))

        ; Exercise 5.8
        (call-exp (rator rands)
          (value-of/k rator env
            (rator-cont rands env cont)))

        (letrec-exp (p-name b-vars p-body letrec-body)
          (value-of/k letrec-body
            (extend-env-rec p-name b-vars p-body env)
            cont))
   )))

  ;; apply-procedure/k : Proc * ExpVal * Cont -> FinalAnswer
  ;; Page 152 and 155
  (define apply-procedure/k
    (lambda (proc1 args cont)
      (cases proc proc1
        (procedure (vars body saved-env)
          (value-of/k body
            (extend-env* vars args saved-env)
            cont)))))


  ;; Page 153 Exercise 5.1
  ;; Implement this data type of continuations using procedual representation.

  ;; Cont = Expval -> FinalAnswer

  (define continuation?
    (lambda (cont)
      (procedure? cont)))

  ;; Page 153 Exercise 5.1
  ;; Implement this data type of continuations using procedual representation.
  ;; apply-cont: Cont * Expval -> FinalAnswer
  (define apply-cont
    (lambda (cont val)
      (cont val)))

  ; end-cont: () -> Cont
  (define end-cont
    (lambda ()
      (lambda (val)
        (begin
          (eopl:printf "End of computation.~%")
          val))))

  ; zeor1-cont : Cont -> Cont
  (define zero1-cont
    (lambda (cont)
      (lambda (val)
        (apply-cont cont
          (bool-val
           (zero? (expval->num val)))))))
                    
  ; let-exp-cont : Var * Exp * Env * Cont -> Cont
  (define let-exp-cont
    (lambda (var body env cont)
      (lambda (val)
        (value-of/k body (extend-env var val env) cont))))
  
  ; if-test-cont: Exp * Exp * Env * Cont -> Cont
  (define if-test-cont
    (lambda (exp2 exp3 env cont)
      (lambda (val)
        (if (expval->bool val)
            (value-of/k exp2 env cont)
            (value-of/k exp3 env cont)))))

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
  
  ; rator-cont : Exps * Env * Cont -> Cont
  (define rator-cont
    (lambda (rands env cont)
      (lambda (val)
        (value-of/k (car rands) env (rand-cont val (cdr rands) '() env cont)))))

  ; rand-cont : ExpVal * Exps * ExpVals * Env * Cont -> Cont
  (define rand-cont
    (lambda (rator rands rand-vals env cont)
      (lambda (val)
        (if (null? rands)
            (apply-procedure/k
              (expval->proc rator)
              (reverse (cons val rand-vals))
              cont)
            (value-of/k (car rands) env
                        (rand-cont rator (cdr rands) (cons val rand-vals) env cont))))))
  )
  


  
