#lang eopl

;Exercise 2.25 [**] Use cases to write max-interior, which takes a binary tree of integers
;(as in the preceding exercise) with at least one interior node and returns the symbol
;associated with an interior node with a maximal leaf sum.

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

;;;;;;;;;;; client-code ;;;;;;;;;;;;;;;;;
; sum-of-btree : Bintree -> Int
(define sum-of-btree
  (lambda (bt)
    (cases bintree bt
      (leaf-node (num) num)
      (interior-node (key left right)
        (+ (sum-of-btree left)
           (sum-of-btree right))))))

; max-interior-helper : bintree * bintree -> bintree
(define max-interior-helper
  (lambda (bt res)
    (cases bintree bt
      (leaf-node (num)
        (if (> num (sum-of-btree res)) bt res))
      (interior-node (key left right)
        (if (> (sum-of-btree bt) (sum-of-btree res))
            (max-interior-helper
             left (max-interior-helper right bt))
            (max-interior-helper
             left (max-interior-helper right res)))))))


; max-interior : Bintree -> symbol
(define max-interior
  (lambda (bt)
    (let ((max-int (max-interior-helper bt (leaf-node -9999999999))))
      (cases bintree max-int
        (leaf-node (num) 'leaf)
        (interior-node (key left right) key)))))


