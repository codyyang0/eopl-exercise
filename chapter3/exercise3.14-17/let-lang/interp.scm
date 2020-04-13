(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the LET language.  The \commentboxes are the
  ;; latex code for inserting the rules into the code in the book.
  ;; These are too complicated to put here, see the text, sorry.

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 71
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 71
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
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of-bool-exp exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
;        (let-exp (var exp1 body)       
;          (let ((val1 (value-of exp1 env)))
;            (value-of body
;              (extend-env var val1 env))))
        (let-exp (vars exps body)
          (let ((vals (map (lambda (exp) (value-of exp env)) exps)))
            (value-of body (extend-env* vars vals env))))

        (let*-exp (vars exps body)
          (if (null? vars)
              (value-of body env)
              (let ((var (car vars))
                    (val (value-of (car exps) env)))
                (value-of (let*-exp (cdr vars) (cdr exps) body)
                          (extend-env var val env)))))

        (print-exp (exp)
           (let ((val (value-of exp env)))
             (cases expval val
               (num-val (num) (eopl:printf "num-val : ~s~%" num))
               (bool-val (boolean) (eopl:printf "bool-val : ~s~%" boolean)))
             (num-val 1)))

        )))

  ; value-of-bool-exp : Bool-exp * Env -> ExpVal
  (define value-of-bool-exp
    (lambda (exp env)
      (cases bool-exp exp
        (zero?-exp (exp)
          (let ((val (value-of exp env)))
            (let ((num (expval->num val)))
              (if (zero? num) (bool-val #t) (bool-val #f)))))
        (equal?-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (bool-val (equal? num1 num2)))))
        (greater?-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (bool-val (> num1 num2)))))
        (less?-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (bool-val (< num1 num2))))))))
  )

