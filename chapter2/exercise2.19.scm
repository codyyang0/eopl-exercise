#lang eopl

; Bintree ::= () | (Int Bintree Bintree)

; constructor
; number->bintree : Int -> bintree
(define number->bintree
  (lambda (n)
    (list n '() '())))

; constructor
; bintree : Int * bintree * bintree -> bintree
(define bintree
  (lambda (n bt1 bt2)
    (list n bt1 bt2)))

; extractor
; current-element : bintree -> Int
(define current-element
  (lambda (bt)
    (car bt)))

; extractor
; move-to-left : bintree -> bintree
(define move-to-left
  (lambda (bt)
    (cadr bt)))

; extractor
; move-to-right : bintree -> bintree
(define move-to-right
  (lambda (bt)
    (caddr bt)))

; pred
; at-leaf? : bintree -> Bool
(define at-leaf? null?)

; constructor
; insert-to-left : Int * bintree -> bintree
(define insert-to-left
  (lambda (n bt)
    (let ((cur-ele (current-element bt))
          (new-left (bintree n (move-to-left bt) '()))
          (right (move-to-right bt)))
      (bintree cur-ele new-left right))))


; constructor
; insert-to-right : Int * bintree -> bintree
(define insert-to-right
  (lambda (n bt)
    (let ((cur-ele (current-element bt))
          (left (move-to-left bt))
          (new-right (bintree n '() (move-to-right bt))))
      (bintree cur-ele left new-right))))

