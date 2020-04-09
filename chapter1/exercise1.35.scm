#lang eopl

;Bintree ::= Int | (Symbol Bintree Bintree)

;Exercise 1.35 [] Write a procedure number-leaves that takes a bintree, and produces a bintree like the original, except the contents of the leaves are numbered starting from 0.

; Note: If just use function params to bind aux-args, it will be so complex. So use variable bound, save a state, will be good.

(define num 0)

;number-leaves : Bintree -> Bintree
(define number-leaves
  (lambda (btree)
    (if (leaf? btree)
        (let ((l (leaf num)))
          (set! num (+ 1 num))
          l)
        (let ((content (contents-of btree)))
          (interior-node content
                         (number-leaves (lson btree))
                         (number-leaves (rson btree)))))))

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