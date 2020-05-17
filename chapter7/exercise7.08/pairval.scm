(module data-structures (lib "eopl.ss" "eopl")

  (require "store.scm")
  (provide (all-defined-out))

  (define-datatype mutpair mutpair?
    (a-pair
     (first reference?)
     (rest reference?)))

  ; make-pair : Expval * Expval -> Mutpair
  (define make-pair
    (lambda (val1 val2)
      (a-pair (newref val1) (newref val2))))

  ; left : Mutpair -> ExpVal
  (define left
    (lambda (p)
      (cases mutpair p
        (a-pair (first rest) (deref first)))))

  ; right : Mutpair -> ExpVal
  (define right
    (lambda (p)
      (cases mutpair p
        (a-pair (first rest) (deref rest)))))

  ; setleft : Mutpair * ExpVal -> Unspecified
  (define setleft
    (lambda (p val)
      (cases mutpair p
        (a-pair (first rest) (setref! first val)))))

   ; setright : Mutpair * ExpVal -> Unspecified
  (define setright
    (lambda (p val)
      (cases mutpair p
        (a-pair (first rest) (setref! rest val)))))
  )
  