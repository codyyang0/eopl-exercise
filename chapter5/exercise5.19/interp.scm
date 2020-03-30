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
          (trampoline
           (value-of/k exp1 (init-env) (end-cont)))))))

  ;; Bounce : ExpVal | () -> Bounce

  ;; trampoline : Bounce -> FinalAnswer
;  (define trampoline
;    (lambda (b)
;      (cases bounce b
;        (expval-bounce (val) val)
;        (proc-bounce (var arg body saved-env cont)
;          (trampoline
;           (value-of/k body (extend-env var arg saved-env) cont))))))

  (define trampoline
    (lambda (bounce)
      (if (expval? bounce)
          bounce
          (trampoline (bounce)))))

  ;; value-of/k : Exp * Env * Cont -> Bounce
  (define value-of/k
    (lambda (exp env cont)
      (cases expression exp
        (const-exp (num) (apply-cont cont (num-val num)))
        
        (var-exp (var) (apply-cont cont (apply-env env var)))

        (diff-exp (exp1 exp2)
          (value-of/k exp1 env
            (diff1-cont (diff1-frame exp2 env) cont)))
        
        (zero?-exp (exp1)
          (value-of/k exp1 env
            (zero1-cont (zero1-frame) cont)))

        (if-exp (exp1 exp2 exp3)
          (value-of/k exp1 env
            (if-test-cont (if-test-frame exp2 exp3 env) cont)))

        (let-exp (var exp1 body)
          (value-of/k exp1 env
            (let-exp-cont (let-exp-frame var body env) cont)))
        
        (proc-exp (var body)
          (apply-cont cont 
            (proc-val (procedure var body env))))

        (call-exp (rator rand) 
          (value-of/k rator env
            (rator-cont (rator-frame rand env) cont)))
        
        (letrec-exp (p-name b-var p-body letrec-body)
          (value-of/k letrec-body
            (extend-env-rec p-name b-var p-body env)
            cont))
   )))

  ;; apply-cont : Cont * ExpVal -> Bounce
  (define apply-cont
    (lambda (cont val)
      (lambda ()
      (if (null? cont) ;; empty-cont
          (begin
            (eopl:printf "End of computation.~%")
            val)
          (let ((fr (car cont))
                (saved-cont (cdr cont)))
            (cases frame fr
              (zero1-frame ()
                (apply-cont saved-cont
                  (bool-val (zero? (expval->num val)))))
              (let-exp-frame (var body saved-env)
                (value-of/k body
                  (extend-env var val saved-env) saved-cont))
              (if-test-frame (exp2 exp3 saved-env)
                (if (expval->bool val)
                    (value-of/k exp2 saved-env saved-cont)
                    (value-of/k exp3 saved-env saved-cont)))
              (diff1-frame (exp2 saved-env)
                (value-of/k exp2
                  saved-env (diff2-cont (diff2-frame val) saved-cont)))
              (diff2-frame (val1)
                (let ((num1 (expval->num val1))
                      (num2 (expval->num val)))
                  (apply-cont saved-cont
                              (num-val (- num1 num2)))))
              (rator-frame (rand saved-env)
                (value-of/k rand saved-env
                  (rand-cont (rand-frame val) saved-cont)))
              (rand-frame (val1)
                (let ((proc (expval->proc val1)))
                  (apply-procedure/k proc val saved-cont)))
        ))))))

  ;; apply-procedure/k : Proc * ExpVal * Cont -> Bounce
  (define apply-procedure/k
    (lambda (proc1 arg cont)
      (cases proc proc1
        (procedure (var body saved-env)
;          (proc-bounce var arg body saved-env cont)))))
         (lambda () (value-of/k body (extend-env var arg saved-env) cont))))))
  )
  


  
