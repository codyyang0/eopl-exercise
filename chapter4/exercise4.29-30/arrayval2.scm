(module arrayval2 (lib "eopl.ss" "eopl")
  
  (require "drscheme-init.scm")
  (require "store.scm")
  
  (provide (all-defined-out))

  (define-datatype array array?
    (an-array
     (refs (list-of reference?))
     (len number?)))

  ;; make-array : ExpVals -> Array
  (define make-array
    (lambda (vals)
      (an-array (newref* vals) (length vals))))

  ;; array-ref : Array Num -> ExpVal
  (define array-ref
    (lambda (arr n)
      (cases array arr
        [an-array (refs len)
         (if (> n (- len 1))
             (eopl:error "Out of Array boundaries")
             (deref (list-ref refs n)))])))

  ;; array-set : Array * Num * ExpVal -> Unspecified
  (define array-set
    (lambda (arr n val)
      (cases array arr
        [an-array (refs len)
         (if (> n (- len 1))
             (eopl:error "Out of Array boundaries")
             (setref! (list-ref refs n) val))])))

  ;; array-length : Array -> number
  (define array-length
    (lambda (arr)
      (cases array arr
        [an-array (refs len) len])))
)
      