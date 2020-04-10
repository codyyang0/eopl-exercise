#lang eopl

; NodeInSequence ::= (Int ListOf(Int) ListOf(Int))

; first define the constructor and extractor, then define the operation upon the interface

; constructor
; number->sequence : Int -> NodeInSequence
(define number->sequence
  (lambda (n)
    (list n '() '())))

(define node-sequence
  (lambda (n loi1 loi2)
    (list n loi1 loi2)))

; extractor
; current-element : NodeInSequence -> Int
(define current-element
  (lambda (nodeInSeq)
    (car nodeInSeq)))

; extractor
; left-seq : NodeInSequence -> NodeInSequence
(define left-seq
  (lambda (nodeInSeq)
    (cadr nodeInSeq)))

; extractor
; right-seq : NodeInSequence -> NodeInSequence
(define right-seq
  (lambda (nodeInSeq)
    (caddr nodeInSeq)))

; constructor
; move-to-left : NodeInSequence -> NodeInSequence
(define move-to-left
  (lambda (nodeInSeq)
    (let ((ele (current-element nodeInSeq))
          (l (left-seq nodeInSeq))
          (r (right-seq nodeInSeq)))
      (if (null? l)
          (report-out-boundance)
          (let ((new-ele (car l))
                (new-left (cdr l))
                (new-right (cons ele r)))
            (node-sequence new-ele new-left new-right))))))

; constructor
; move-to-right : NodeInSequence -> NodeInSequence
(define move-to-right
  (lambda (nodeInSeq)
    (let ((ele (current-element nodeInSeq))
          (l (left-seq nodeInSeq))
          (r (right-seq nodeInSeq)))
      (if (null? r)
          (report-out-boundance)
          (let ((new-ele (car r))
                (new-left (cons ele l))
                (new-right (cdr r)))
            (node-sequence new-ele new-left new-right))))))

; constructor
; insert-into-left : Int * NodeInSequence -> NodeInSequence
(define insert-to-left
  (lambda (n nodeInSeq)
    (let ((ele (current-element nodeInSeq))
          (l (cons n (left-seq nodeInSeq)))
          (r (right-seq nodeInSeq)))
      (node-sequence ele l r))))

; constructor
; insert-into-right : Int * NodeInSequence -> NodeInSequence
(define insert-to-right
  (lambda (n nodeInSeq)
    (let ((ele (current-element nodeInSeq))
          (l (left-seq nodeInSeq))
          (r (cons n (right-seq nodeInSeq))))
      (node-sequence ele l r))))

(define report-out-boundance
  (lambda ()
    (eopl:error 'move-out-boundance
                 "Can't move out of the boundance")))
          


