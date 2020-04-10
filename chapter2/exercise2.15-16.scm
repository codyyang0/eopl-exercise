#lang eopl

;exercise 2.15-2.16
;Exercise 2.15 [*] Implement the lambda-calculus expression interface for the representation specified by the grammar above.

;;;;;;;;;;constructor;;;;;;;;;;;;;

; var-exp : var -> LcExp
(define var-exp
  (lambda (var)
    (list 'var var)))

; lambda-exp : var * LcExp -> LcExp
(define lambda-exp
  (lambda (var lcExp)
    (list 'lambda (list var) lcExp)))
; exercise 2.16
;    (list 'lambda var lcExp)))

; app-exp : LcExp1 * LcExp2 -> LcExp
(define app-exp
  (lambda (lcExp1 lcExp2)
    (list 'app lcExp1 lcExp2)))

;;;;;;;;;; predicates ;;;;;;;;;;;;;;;;;;;

; var-exp? : lcExp -> boolean
(define var-exp?
  (lambda (lcExp)
    (eqv? (car lcExp) 'var)))

; lambda-exp? : LcExp -> boolean
(define lambda-exp?
  (lambda (lcExp)
    (eqv? (car lcExp) 'lambda)))

; app-exp? : LcExp -> boolean
(define app-exp?
  (lambda (lcExp)
    (eqv? (car lcExp) 'app)))

;;;;;;;;; extractors ;;;;;;;;;;;;;;;;;;;

; var-exp->var : LcExp -> Var
(define var-exp->var
  (lambda (lcExp)
    (cadr lcExp)))

; lambda-exp->bound-var : LcExp -> Var
(define lambda-exp->bound-var
  (lambda (lcExp)
    (caadr lcExp)))
; exercise 2.16
;    (cadr lcExp)))

; lambda-exp->body : LcExp -> LcExp
(define lambda-exp->body
  (lambda (lcExp)
    (caddr lcExp)))

; app-exp->rator : LcExp -> LcExp
(define app-exp->rator
  (lambda (lcExp)
    (cadr lcExp)))

; app-exp->rand : LcExp -> LcExp
(define app-exp->rand
  (lambda (lcExp)
    (caddr lcExp)))

; client code : pattern for each kind of data in the LcExp datatype
; occurs-free? : Sym * LcExp -> Bool
(define occurs-free?
  (lambda (search-var exp)
    (cond
      ((var-exp? exp) (eqv? (var-exp->var exp) search-var))
      ((lambda-exp? exp)
       (and
        (not (eqv? search-var (lambda-exp->bound-var exp)))
        (occurs-free? search-var (lambda-exp->body exp))))
      (else
       (or
        (occurs-free? search-var (app-exp->rator exp))
        (occurs-free? search-var (app-exp->rand exp)))))))
