(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the EXPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  
  (provide value-of-program value-of instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 110
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (cases answer (value-of exp1 (init-env) (initialize-store!))
            [an-answer [val store] val])))))

  ;; value-of : Exp * Env * Store -> Answer
  ;; Page: 113
  (define value-of
    (lambda (exp env store)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (an-answer (num-val num) store))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var)
          (an-answer
           ;(apply-store store (apply-env env var)) store))
           (apply-env env var) store))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (cases answer (value-of exp1 env store)
            [an-answer (val1 store1)
              (cases answer (value-of exp2 env store1)
                [an-answer (val2 store2)
                  (let ([num1 (expval->num val1)]
                        [num2 (expval->num val2)])
                    [an-answer [num-val (- num1 num2)] store2])])]))
      
        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (cases answer (value-of exp1 env store)
            [an-answer [val1 new-store]
              (let [[num1 (expval->num val1)]]
                (if (zero? num1)
                    [an-answer [bool-val #t] new-store]
                    [an-answer [bool-val #f] new-store]))]))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (cases answer (value-of exp1 env store)
            [an-answer [val1 new-store]
              (if (expval->bool val1)
                  (value-of exp2 env new-store)
                  (value-of exp3 env new-store))]))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)
          (cases answer (value-of exp1 env store)
            [an-answer [val1 new-store]
              (value-of body (extend-env var val1 env) new-store)]))
        
        (proc-exp (var body)
          (an-answer (proc-val (procedure var body env)) store))

        (call-exp (rator rand)
          (cases answer (value-of rator env store)
            [an-answer [rator-val store1]
              (cases answer (value-of rand env store1)
                [an-answer (arg store2)
                  (let [[proc (expval->proc rator-val)]]
                    (apply-procedure proc arg store2))])]))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env) store))

        (begin-exp (exp1 exps)
          (letrec 
            [[value-of-begins
               (lambda (e1 es store)
                 (cases answer (value-of e1 env store)
                   [an-answer [v1 new-store]
                     (if (null? es)
                         [an-answer v1 new-store]
                         (value-of-begins (car es) (cdr es) new-store))]))]]
            (value-of-begins exp1 exps store)))

        (newref-exp (exp1)
          (cases answer (value-of exp1 env store)
            [an-answer [v1 new-store]
              (let [[res (newref v1 new-store)]]
                (let [[ref (car res)]
                      [new-store (cadr res)]]
                  (begin
                    ;(eopl:printf "ref : ~s~%" ref)
                    [an-answer [ref-val ref] new-store])))]))

        (deref-exp (exp1)
          (cases answer (value-of exp1 env store)
            [an-answer [v1 new-store]
              (begin
                ;(eopl:printf "val : ~s~%" v1)
                (let [[ref1 (expval->ref v1)]]
                  [an-answer (deref ref1 new-store) new-store]))]))

        (setref-exp (exp1 exp2)
          (cases answer (value-of exp1 env store)
            [an-answer [v1 store1]
              (let [[ref (expval->ref v1)]]
                (cases answer (value-of exp2 env store1)
                  [an-answer [v2 store2]
                    (begin
                      ;(eopl:printf "ref : val - ~s : ~s~%" ref v2)
                      (an-answer (num-val 23) (setref! ref v2 store2)))]))]))
        )))

  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; 
  ;; uninstrumented version
  ;;   (define apply-procedure
  ;;    (lambda (proc1 arg)
  ;;      (cases proc proc1
  ;;        (procedure (bvar body saved-env)
  ;;          (value-of body (extend-env bvar arg saved-env))))))

  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 arg store)
      (cases proc proc1
        (procedure (var body saved-env)
	  (let ((r arg))
	    (let ((new-env (extend-env var r saved-env)))
	      (when (instrument-let)
		(begin
		  (eopl:printf
		    "entering body of proc ~s with env =~%"
		    var)
		  (pretty-print (env->list new-env))
                  (eopl:printf "store =~%")
                  (pretty-print (store->readable (get-store-as-list)))
                  (eopl:printf "~%")))
              (value-of body new-env store)))))))

  ;; apply-store : store * expval -> expval
  ; if expval is ref-val, then find the value in the store, else return expval
  (define apply-store
    (lambda (store val)
      (cases expval val
        [ref-val [ref] (deref ref store)]
        [else val])))

  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (cons
            (car p)
            (expval->printable (cadr p))))
        l)))

  ;; answer datatype
  (define-datatype answer answer?
    [an-answer
     [val expval?]
     [store store?]])
 
  )
  


  
