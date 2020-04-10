#lang eopl
;Exercise 2.12 [*] Implement the stack data type of exercise 2.4 using a procedural representation.

; note : datatype only focus the interface operation, replesentations can be different.


; stack = symbol -> schemeVal

; 3 observer : pop top empty-stack? so the constructor need supply 3 operation
; 1 observer dispatch : apply-stack

;constructor
;empty-stack : () -> stack
(define empty-stack
  (lambda ()
    (lambda (op)
      (if (eqv? op 'empty-stack?)
          #t
          (report-error-stack-op op)))))

;constructor
;push : schemeVal * stack -> stack
(define push
  (lambda (val stack)
    (lambda (op)
      (cond ((eqv? op 'pop) stack)
            ((eqv? op 'top) val)
            ((eqv? op 'empty-stack?) #f)
            (else
             (report-error-invalid-oper op))))))

;observer
;pop : stack -> stack
(define pop
  (lambda (stack)
    (apply-stack 'pop stack)))

;observer
;top : stack -> schemeVal
(define top
  (lambda (stack)
    (apply-stack 'top stack)))

;observer
;empty-stack? : stack -> boolean
(define empty-stack?
  (lambda (stack)
    (apply-stack 'empty-stack? stack)))

;observer of dispatch
;apply-stack : symbol * stack -> schemeVal | stack | boolean
(define apply-stack
  (lambda (op stack)
    (stack op)))
              
(define report-error-stack-op
  (lambda (op)
    (eopl:error 'empty-stack
                "empty-stack can't ~s~%" op)))

(define report-error-invalid-oper
  (lambda (op)
    (eopl:error 'stack-operation
                "error operation ~s" op)))