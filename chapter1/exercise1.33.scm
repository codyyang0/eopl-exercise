#lang eopl

;Bintree ::= Int | (Symbol Bintree Bintree)

;Exercise 1.33 [ ] Write a procedure mark-leaves-with-red-depth that takes a bintree (definition 1.1.7), and produces a bintree of the same shape as the original, except that in the new tree, each leaf contains the integer of nodes between it and the root that contain the symbol red. For example, the expression.

; mark-leaves-with-label-depth : Symbol * Bintree -> Bintree
(define mark-leaves-with-label-depth
  (lambda (sym btree)
    (mark-helper sym btree 0)))

; mark-helper : Symbol * Bintree * Int -> Bintree
(define mark-helper
  (lambda (sym btree n)
    (if (leaf? btree)
        n
        (let ((content (contents-of btree)))
          (if (eqv? content sym)
              (interior-node content
                             (mark-helper sym (lson btree) (+ 1 n))
                             (mark-helper sym (rson btree) (+ 1 n)))
              (interior-node content
                             (mark-helper sym (lson btree) n)
                             (mark-helper sym (rson btree) n)))))))

;mark-leaves-with-red-depth : Bintree -> Bintree
(define mark-leaves-with-red-depth
  (lambda (btree)
    (mark-leaves-with-label-depth 'red btree)))


;leaf : Int -> Int
(define leaf
  (lambda (i) i))

;interior-node : symbol * Bintree * Bintree
(define interior-node
  (lambda (sym btree1 btree2)
    (list sym btree1 btree2)))

; leaf? : Bintree -> boolean
(define leaf? number?)

; lson : Bintree -> Bintree
(define lson
  (lambda (btree)
    (if (leaf? btree)
        (report-have-no-son btree 'lson)
        (cadr btree))))

; rson : Bintree -> Bintree
(define rson
  (lambda (btree)
    (if (leaf? btree)
        (report-have-no-son btree 'rson)
        (caddr btree))))

; contents-of : Bintree -> Symbol|Int
(define contents-of
  (lambda (btree)
    (if (leaf? btree)
        btree
        (car btree))))

(define report-have-no-son
  (lambda (btree op)
    (eopl:error 'l-r-son
                "Leaf ~s have no ~s~%" btree op)))

(define bt
  (interior-node
   'red
   (interior-node 'bar (leaf 26) (leaf 12))
   (interior-node 'red (leaf 11)
                  (interior-node 'quux
                                 (leaf 117)
                                 (leaf 14)))))
