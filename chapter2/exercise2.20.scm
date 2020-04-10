#lang eopl

;Exercise 2.20 [***] In the representation of binary trees in exercise 2.19 it is easy to
;move from a parent node to one of its sons, but it is impossible to move from a son to
;its parent without the help of context arguments. Extend the representation of lists in
;exercise 2.18 to represent nodes in a binary tree. As a hint, consider representing the
;portion of the tree above the current node by a reversed list, as in exercise 2.18. In
;this representation, implement the procedures from exercise 2.19. Also implement move-up,
;at-root?, and at-leaf?.


;;;;;;;;;;;; node ;;;;;;;;;;;;;;
; node ::= () | (Int node node)

; number->node : Int -> node
(define number->node
  (lambda (n)
    (list n '() '())))

; node : Int * node * node -> node
(define node
  (lambda (n l-node r-node)
    (list n l-node r-node)))

; empty-node
(define empty-node '())

; current-node-val : node -> Int
(define current-node-val
  (lambda (nod)
    (car nod)))

; move-to-left-node : node -> node
(define move-to-left-node
  (lambda (nod)
    (cadr nod)))

; move-to-right-node : node -> node
(define move-to-right-node
  (lambda (nod)
    (caddr nod)))


;;;;;;;;;;;; Bintree ;;;;;;;;;;;;;

; Bintree ::= (node ListOf(node))

; number->bintree : Int -> Bintree
(define number->bintree
  (lambda (n)
    (list (number->node n) '(()))))

; current-element : Bintree -> node
(define current-element
  (lambda (bt)
    (car bt)))

; parents : Bintree -> ListOf(node)
(define parents
  (lambda (bt)
    (cadr bt)))

; bintree : node * ListOf(node) -> Bintree
(define bintree
  (lambda (node pars)
    (list node pars)))

; move-to-left : Bintree -> Bintree
(define move-to-left
  (lambda (bt)
    (if (at-leaf? bt)
        (report-move-out-leaf)
        (let ((cur-node (current-element bt))
              (pars (parents bt)))
          (let ((new-cur-node (move-to-left-node cur-node))
                (new-pars (cons cur-node pars)))
            (bintree new-cur-node new-pars))))))

; move-to-right : Bintree -> Bintree
(define move-to-right
  (lambda (bt)
    (if (at-leaf? bt)
        (report-move-out-leaf)
        (let ((cur-node (current-element bt))
              (pars (parents bt)))
          (let ((new-cur-node (move-to-right-node cur-node))
                (new-pars (cons cur-node pars)))
            (bintree new-cur-node new-pars))))))

; at-leaf? : Bintree -> Bool
(define at-leaf?
  (lambda (bt)
    (null? (current-element bt))))

; at-root? : Bintree -> Bool
(define at-root?
  (lambda (bt)
    (null? (car (parents bt)))))

; insert-to-left : node * Bintree -> Bintree
(define insert-to-left
  (lambda (nod bt)
    (let ((cur-node (current-element bt))
          (pars (parents bt)))
      (let ((val (current-node-val cur-node))
            (l-node (move-to-left-node cur-node))
            (r-node (move-to-right-node cur-node)))
        (let ((new-l-node (node (current-node-val nod) l-node empty-node)))
          (let ((new-node (node val new-l-node r-node)))
            (bintree new-node pars)))))))

; insert-to-right : node * Bintree -> Bintree
(define insert-to-right
  (lambda (nod bt)
    (let ((cur-node (current-element bt))
          (pars (parents bt)))
      (let ((val (current-node-val cur-node))
            (l-node (move-to-left-node cur-node))
            (r-node (move-to-right-node cur-node)))
        (let ((new-r-node (node (current-node-val nod) empty-node r-node)))
          (let ((new-node (node val l-node new-r-node)))
            (bintree new-node pars)))))))

; move-up : Bintree -> Bintree
(define move-up
  (lambda (bt)
    (if (at-root? bt)
        (report-move-up-root)
        (let ((cur-node (car (parents bt)))
              (new-pars (cdr (parents bt))))
          (bintree cur-node new-pars)))))


(define report-move-out-leaf
  (lambda ()
    (eopl:error 'move-out-leaf
                 "Can't move left|right of the leaf")))

(define report-move-up-root
  (lambda ()
    (eopl:error 'move-up
                 "Can't move up on from root")))