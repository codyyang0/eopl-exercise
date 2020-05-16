(module arrayval1 (lib "eopl.ss" "eopl")
  
  (require "drscheme-init.scm")
  (require "store.scm")
  
  (provide (all-defined-out))

  (define-datatype array array?
    (an-array
     (refs (list-of reference?))))

  ;; make-array : ExpVals -> Array
  (define make-array
    (lambda (vals)
      (an-array (newref* vals))))

  ;; array-ref : Array Num -> ExpVal
  (define array-ref
    (lambda (arr n)
      (cases array arr
        [an-array (a)
         (if (> n (- (length a) 1))
             (eopl:error "Out of Array boundaries")
             (deref (list-ref a n)))])))

  ;; array-set : Array * Num * ExpVal -> Unspecified
  (define array-set
    (lambda (arr n val)
      (cases array arr
        [an-array (a)
         (if (> n (- (length a) 1))
             (eopl:error "Out of Array boundaries")
             (setref! (list-ref a n) val))])))
)
      