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
      (proc proc?))
    (cons-val
     (val1 expval?)
     (val2 expval?))
    (emptylist-val)
    )

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

  (define expval->car
    (lambda (v)
      (cases expval v
        (cons-val (val1 val2) val1)
        (else (expval-extractor-error 'car v)))))

  (define expval->cdr
    (lambda (v)
      (cases expval v
        (cons-val (val1 val2) val2)
        (else (expval-extractor-error 'cdr v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))

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


;;;;;;;;;;;; cons ;;;;;;;;;;;;;;;;

  (define cons-car
    (lambda (pair)
      (cases expval pair
        (cons-val (val1 val2) val1)
        (else (eopl:error 'cons-car "~s is not list" pair)))))
        
  (define cons-cdr
    (lambda (pair)
      (cases expval pair
        (cons-val (val1 val2) val2)
        (else (eopl:error 'cons-cdr "~s is not list" pair)))))

  (define cons-null?
    (lambda (p)
      (cases expval p
        (emptylist-val () (bool-val #t))
        (else (bool-val #f)))))

  (define empty-list
    (emptylist-val))
)
