#lang eopl

;Exercise 1.31 [*] Write the following procedures for calculating on a bintree (definition 1.1.7): leaf and interior-node, which build bintrees, leaf?, which tests whether a bintree is a leaf, and lson, rson, and contents-of, which extract the components of a node. contents-of should work on both leaves and interior nodes.

;Bintree ::= Int | (Symbol Bintree Bintree)

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