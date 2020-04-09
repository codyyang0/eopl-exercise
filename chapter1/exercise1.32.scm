#lang eopl

;Exercise 1.32 [*] Write a procedure double-tree that takes a bintree, as represented in definition 1.1.7, and produces another bintree like the original, but with all the integers in the leaves doubled.

;Bintree ::= Int | (Symbol Bintree Bintree)

; double-tree : Bintree -> Bintree
(define double-tree
  (lambda (btree)
    (if (leaf? btree)
        (* 2 btree)
        (interior-node (contents-of btree)
                       (double-tree (lson btree))
                       (double-tree (rson btree))))))


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
  (interior-node 'root
                 (interior-node 'l1
                                (leaf 1)
                                (leaf 2))
                 (interior-node 'r1
                                (leaf 3)
                                (leaf 4))))