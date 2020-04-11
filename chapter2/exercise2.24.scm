#lang eopl

;exercise 2.24
;Implement a bintree-to-list procedure for binary trees, so that
;(bintree-to- list (interior-node 'a (leaf-node 3) (leaf-node 4))) returns the list
;(interior-node a (leaf-node 3) (leaf-node 4))

; constructor
(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

; observer
(define bintree-to-list
  (lambda (bt)
    (cases bintree bt
      (leaf-node (num) (list 'leaf-node num))
      (interior-node (key left right)
        (list 'interior-node key
              (bintree-to-list left)
              (bintree-to-list right))))))
