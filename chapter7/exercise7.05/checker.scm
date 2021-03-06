(module checker (lib "eopl.ss" "eopl")

  (require "drscheme-init.scm")
  (require "lang.scm")

  (provide type-of type-of-program)

  ;; check-equal-type! : Type * Type * Exp -> Unspecified
  ;;                     Types * Types * Exp -> Unspecified
  ;; Page: 242
  (define check-equal-type!
    (lambda (ty1 ty2 exp)
      (when (not (equal? ty1 ty2))
        (report-unequal-types ty1 ty2 exp))))

  ;; report-unequal-types : Type * Type * Exp -> Unspecified
  ;; Page: 243
  (define report-unequal-types
    (lambda (ty1 ty2 exp)
      (eopl:error 'check-equal-type!  
          "Types didn't match: ~s != ~a in~%~a"
          (type-to-external-form ty1)
          (type-to-external-form ty2)
          exp)))

  ;;;;;;;;;;;;;;;; The Type Checker ;;;;;;;;;;;;;;;;
  
  ;; type-of-program : Program -> Type
  ;; Page: 244
  (define type-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1) (type-of exp1 (init-tenv))))))

  ;; type-of : Exp * Tenv -> Type
  ;; Page 244--246
  (define type-of
    (lambda (exp tenv)
      (cases expression exp

        ;; \commentbox{\hastype{\tenv}{\mv{num}}{\mathtt{int}}}
        (const-exp (num) (int-type))

        ;; \commentbox{\hastype{\tenv}{\var{}}{\tenv{}(\var{})}}
        (var-exp (var) (apply-tenv tenv var))

        ;; \commentbox{\diffrule}
        (diff-exp (exp1 exp2)
          (let ((ty1 (type-of exp1 tenv))
                (ty2 (type-of exp2 tenv)))
            (check-equal-type! ty1 (int-type) exp1)
            (check-equal-type! ty2 (int-type) exp2)
            (int-type)))

        ;; \commentbox{\zerorule}
        (zero?-exp (exp1)
          (let ((ty1 (type-of exp1 tenv)))
            (check-equal-type! ty1 (int-type) exp1)
            (bool-type)))

        ;; \commentbox{\condrule}
        (if-exp (exp1 exp2 exp3)
          (let ((ty1 (type-of exp1 tenv))
                (ty2 (type-of exp2 tenv))
                (ty3 (type-of exp3 tenv)))
            (check-equal-type! ty1 (bool-type) exp1)
            (check-equal-type! ty2 ty3 exp)
            ty2))

        ;; \commentbox{\letrule}
        (let-exp (vars exps body)
          (let ((exp-types (map (lambda (exp) (type-of exp tenv)) exps)))
            (type-of body
              (extend-tenv* vars exp-types tenv))))

        ;; \commentbox{\procrulechurch}
        (proc-exp (vars var-types body)
          (let ((result-type
                  (type-of body
                    (extend-tenv* vars var-types tenv))))
            (proc-type var-types result-type)))

        ;; \commentbox{\apprule}
        (call-exp (rator rands) 
          (let ((rator-type (type-of rator tenv))
                (rand-types (map (lambda (rand) (type-of rand tenv)) rands)))
            (cases type rator-type
              (proc-type (arg-types result-type)
                (begin
                  (check-equal-type! arg-types rand-types rands)
                  result-type))
              (else
                (report-rator-not-a-proc-type rator-type rator)))))

        ;; \commentbox{\letrecrule}
        (letrec-exp (p-result-type p-name b-vars b-var-types p-body
                      letrec-body)
          (let ((tenv-for-letrec-body
                  (extend-tenv p-name
                    (proc-type b-var-types p-result-type)
                    tenv)))
            (let ((p-body-type 
                    (type-of p-body
                      (extend-tenv* b-vars b-var-types
                        tenv-for-letrec-body)))) 
              (check-equal-type!
                p-body-type p-result-type p-body)
              (type-of letrec-body tenv-for-letrec-body)))))))
    
  (define report-rator-not-a-proc-type
    (lambda (rator-type rator)
      (eopl:error 'type-of-expression
        "Rator not a proc type:~%~s~%had rator type ~s"   
           rator 
           (type-to-external-form rator-type))))

  ;;;;;;;;;;;;;;;; type environments ;;;;;;;;;;;;;;;;
    
  (define-datatype type-environment type-environment?
    (empty-tenv-record)
    (extended-tenv-record
      (sym symbol?)
      (type type?)
      (tenv type-environment?))
    (extended-tenv-record*
     (syms (list-of symbol?))
     (types (list-of type?))
     (tenv type-environment?)))
    
  (define empty-tenv empty-tenv-record)
  (define extend-tenv extended-tenv-record)
  (define extend-tenv* extended-tenv-record*)
    
    
  (define apply-tenv 
    (lambda (tenv sym)
      (cases type-environment tenv
        (empty-tenv-record ()
          (eopl:error 'apply-tenv "Unbound variable ~s" sym))
        (extended-tenv-record (sym1 val1 old-env)
          (if (eqv? sym sym1) 
            val1
            (apply-tenv old-env sym)))
        (extended-tenv-record* (syms vals old-env)
          (let [[n (location syms sym)]]
            (if n
                (list-ref vals n)
                (apply-tenv old-env sym)))))))
  
  (define init-tenv
    (lambda ()
      (extend-tenv 'x (int-type) 
        (extend-tenv 'v (int-type)
          (extend-tenv 'i (int-type)
            (empty-tenv))))))

  (define location
    (lambda (syms search-sym)
      (cond
        [(null? syms) #f]
        [(eqv? search-sym (car syms)) 0]
        [(location (cdr syms) search-sym)
         => (lambda (n)
              (+ n 1))]
        [else #f])))
          
             

  )
