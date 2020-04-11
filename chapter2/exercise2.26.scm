#lang eopl

;Exercise 2.26 [ ] Here is another version of exercise 1.33. Consider a set of trees given by the following grammar:
;Red-blue-tree ::= Red-blue-subtree 
;Red-blue-subtree::=(red-node Red-blue-subtree Red-blue-subtree)
;                ::=(blue-node {Red-blue-subtree}∗) 
;                ::=(leaf-node Int)
;Write an equivalent definition using define-datatype, and use the resulting inter- face to write a
;procedure that takes a tree and builds a tree of the same shape, except that each leaf node is replaced
;by a leaf node that contains the number of red nodes on the path between it and the root.

; constructor
(define-datatype red-blue-tree red-blue-tree?
  (a-red-blue-tree
   (subtree red-blue-subtree?)))

; constructor
(define-datatype red-blue-subtree red-blue-subtree?
  (red-node
   (left-node red-blue-subtree?)
   (right-node red-blue-subtree?))
  (blue-node
   (nodes (list-of red-blue-tree?)))
  (leaf-node
   (num integer?)))









