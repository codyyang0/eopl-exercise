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
        
        (add-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (+ num1 num2)))))

        (mult-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (* num1 num2)))))

        (quot-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (/ num1 num2)))))

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

        (minus-exp (exp)
          (let ((val (value-of exp env)))
            (num-val (- (expval->num val)))))

        (equal?-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (bool-val (= num1 num2)))))

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
              (bool-val (< num1 num2)))))

        (emptylist-exp () (emptylist-val))

        (cons-exp (exp1 exp2)
          (pair-val (value-of exp1 env) (value-of exp2 env)))

        ; get the car
        (car-exp (exp)
          (let ((val (value-of exp env)))
            (cases expval val
              (pair-val (car cdr) car)
              (else
               (report-invalid-list 'car)))))

        ; get the cdr
        (cdr-exp (exp)
          (let ((val (value-of exp env)))
            (cases expval val
              (pair-val (car cdr) cdr)
              (else
               (report-invalid-list 'cdr)))))
        
        ; emptylist-val is true, others is false
        (null?-exp (exp)
          (let ((val (value-of exp env)))
            (cases expval val
              (emptylist-val () #t)
              (else #f))))

        ;list-exp's value is a pair-val
        (list-exp (exps)
          (if (null? exps)
              (emptylist-val)
              (pair-val (value-of (car exps) env)
                        (value-of (list-exp (cdr exps)) env))))

        (cond-exp (pred-exps result-exps)
           (if (null? pred-exps)
               (report-cond-error)
               (let ((cur-pred-exp (car pred-exps))
                     (cur-result-exp (car result-exps))
                     (rest-pred-exps (cdr pred-exps))
                     (rest-result-exps (cdr result-exps)))
                 (if (expval->bool (value-of cur-pred-exp env))
                     (value-of cur-result-exp env)
                     (value-of (cond-exp rest-pred-exps rest-result-exps) env)))))

        )))

  (define report-invalid-list
   (lambda (op)
     (eopl:error 'invalid-list
                 "Can't ~s on an invalid list" op)))

  (define report-cond-error
    (lambda ()
      (eopl:error 'cond-error
                  "Every pred is false, cond error")))
                 
  )

