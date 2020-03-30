(module data-structures (lib "eopl.ss" "eopl")

  (require "lang.scm")                  ; for expression?
  (provide (all-defined-out))           ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean or a procval.

  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (proc-val 
      (proc proc?)))

;;; extractors:

  (define expval->num
    (lambda (v)
      (cases expval v
	(num-val (num) num)
	(else (expval-extractor-error 'num v)))))

  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (expval-extractor-error 'bool v)))))

  (define expval->proc
    (lambda (v)
      (cases expval v
	(proc-val (proc) proc)
	(else (expval-extractor-error 'proc v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))

;;;;;;;;;;;;;;;; frames ;;;;;;;;;;;;;;;;

  ;; Page: 148
  (define identifier? symbol?)

  (define-datatype frame frame?
    (zero1-frame)              
    (let-exp-frame
     (var identifier?)
     (body expression?)
     (saved-env environment?))
    (if-test-frame
      (exp2 expression?)
      (exp3 expression?)
      (saved-env environment?))
    (diff1-frame              
      (exp2 expression?)
      (saved-env environment?))
    (diff2-frame                
      (val1 expval?))
    (rator-frame            
      (rand expression?)
      (saved-env environment?))
    (rand-frame            
      (val1 expval?)))

  ;;;;;;;;;;;;;;;; continuations ;;;;;;;;;;;;;;;;
  (define end-cont (lambda () '()))
  
  (define zero1-cont
    (lambda (zero1-frame cont)
      (cons zero1-frame cont)))
  
  (define let-exp-cont
    (lambda (let-exp-frame cont)
      (cons let-exp-frame cont)))

  (define if-test-cont
    (lambda (if-test-frame cont)
      (cons if-test-frame cont)))

  (define diff1-cont
    (lambda (diff1-frame cont)
      (cons diff1-frame cont)))

  (define diff2-cont
    (lambda (diff2-frame cont)
      (cons diff2-frame cont)))

  (define rator-cont
    (lambda (rator-frame cont)
      (cons rator-frame cont)))

  (define rand-cont
    (lambda (rand-frame cont)
      (cons rand-frame cont)))
  

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  (define-datatype proc proc?
    (procedure
      (bvar symbol?)
      (body expression?)
      (env environment?)))
  
;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;

  (define-datatype environment environment?
    (empty-env)
    (extend-env 
      (bvar symbol?)
      (bval expval?)
      (saved-env environment?))
    (extend-env*
     (bvars (list-of symbol?))
     (bvals (list-of expval?))
     (saved-env environment?))
    (extend-env-rec
      (p-name symbol?)
      (b-var symbol?)
      (p-body expression?)
      (saved-env environment?)))


  ;;;;;;;;;;;; bounce structures ;;;;;;;;;;;;;;;;;;;;;;
  (define-datatype bounce bounce?
    (expval-bounce
     (val expval?))
    (proc-bounce
     (var identifier?)
     (arg expval?)
     (body expression?)
     (saved-env environment?)
     (cont list?)))  ; stack
 
)
