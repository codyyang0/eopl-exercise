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
      (init-env!)
      (cases program pgm
        (a-program (exp1)
          (value-of/k exp1 (end-cont))))))  

  ;; value-of/k : Exp * Env * Cont -> FinalAnswer
  ;; Page: 143--146, and 154
  (define value-of/k
    (lambda (exp cont)
      (cases expression exp
        (const-exp (num) (apply-cont cont (num-val num)))
        
        (var-exp (var) (apply-cont cont (deref (apply-env var))))
        
        (diff-exp (exp1 exp2)
          (value-of/k exp1
            (diff1-cont exp2 cont)))
        
        (zero?-exp (exp1)
          (value-of/k exp1
            (zero1-cont cont)))
        
        (if-exp (exp1 exp2 exp3)
          (value-of/k exp1
            (if-test-cont exp2 exp3 cont)))
        
        (let-exp (var exp body)
          (value-of/k exp (let-exp-cont var body cont)))
        
        (proc-exp (var body)
          (apply-cont cont 
            (proc-val (procedure var body (get-env)))))
        
        (call-exp (rator rand) 
          (value-of/k rator
            (rator-cont rand cont)))
        
        (letrec-exp (p-name b-var p-body letrec-body)
          (let ((cur-env (get-env)))
            (extend-env-rec! p-name b-var p-body)
          (let ((val (value-of/k letrec-body cont))
                (env (get-env)))
            (set! env cur-env)
            val)))
            

        ; Exercise 5.9
        (assign-exp (var exp)
          (value-of/k exp (set-rhs-cont cont)))
   )))

  ;; apply-procedure/k : Proc * ExpVal * Cont -> FinalAnswer
  ;; Page 152 and 155
  (define apply-procedure/k
    (lambda (proc1 arg cont)
      (cases proc proc1
        (procedure (var body saved-env)
          (let ((cur-env (get-env)))
            (extend-env! var (newref arg))
            (let ((val (value-of/k body cont))
                  (env (get-env)))
              (set! env cur-env)
              val))))))
                           

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
  
  ; let-exp-cont : Var * Exp * Cont -> Cont
  (define let-exp-cont
    (lambda (var body cont)
      (lambda (val)
        (let ((saved-env (get-env)))
          (extend-env! var (newref val))
          (let ((body-val (value-of/k body cont))
                (env (get-env)))
            (set! env saved-env)
            body-val)))))
          
;          (let ((env (get-env))
;                (val (value-of/k body cont)))
;            (set! env saved-env)
;            val)))))

  ;if-test-cont : Exp * Exp * Cont -> Cont
  (define if-test-cont
    (lambda (exp2 exp3 cont)
      (lambda (val)
        (if (expval->bool val)
            (value-of/k exp2 cont)
            (value-of/k exp3 cont)))))

  ; apply-cont : Cont * ExpVal -> FinalAnswer
  (define apply-cont
    (lambda (cont v)
      (cont v)))

  ; diff1-cont : Exp * Cont -> Cont
  (define diff1-cont
    (lambda (exp2 cont)
      (lambda (val1)
        (value-of/k exp2 (diff2-cont val1 cont)))))

  (define diff2-cont
    (lambda (val1 cont)
      (lambda (val2)
        (apply-cont cont
                    (num-val (- (expval->num val1)
                                (expval->num val2)))))))
  (define rator-cont
    (lambda (rand cont)
      (lambda (val)
        (value-of/k rand (rand-cont val cont)))))

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
           (apply-env var)
           val)
          (num-val 27)))))

  )
  


  
