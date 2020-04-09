#lang eopl

;Exercise 1.34 [***] Write a procedure path that takes an integer n and a binary search tree bst (page 10) that contains the integer n, and returns a list of lefts and rights showing how to find the node containing n. If n is found at the root, it returns the empty list.

; Binary-search-tree ::= () | (Int Binary-search-tree Binary-search-tree)


; path : Int * BST -> List | #f (if not find the int return #f)
(define path
  (lambda (i bst)
    (path-helper i bst '())))

; path-helper : Int * BST * ListOf(Symbol) -> ListOf(Symbol) | #f
(define path-helper
  (lambda (i bst path)
    (if (null? bst)
        #f
        (cond ((= (car bst) i) path)
              ((< (car bst) i) (path-helper i (caddr bst) (append path (list 'right))))
              (else
               (path-helper i (cadr bst) (append path (list 'left))))))))
