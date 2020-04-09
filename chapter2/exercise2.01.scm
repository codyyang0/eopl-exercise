#lang eopl

;Exercise 2.1 [*] Implement the four required operations for bigits. Then use your implementation to calculate the factorial of 10. How does the execution time vary as this argument changes? How does the execution time vary as the base changes? Explain why.

; zero : -> num
(define zero
  (lambda () '()))

; is-zero? : num -> boolean
(define is-zero? null?)

(define base 15)

;successor : num -> num
(define successor
  (lambda (n)
    (if (is-zero? n)
        '(1)
        (let ((r (car n))
              (q (cdr n)))
          (if (= r base)
              (cons 0 (successor q))
              (cons (+ r 1) q))))))

;predecessor : num -> num
(define predecessor
  (lambda (n)
    (if (is-zero? n)
        (report-error-zero-decr)
        (let ((r (car n))
              (q (cdr n)))
          (if (not (is-zero? q))
              (if (= r 0)
                  (cons base (predecessor q))
                  (cons (- r 1) q))
              (cond
                ((= 0 r) (report-error-zero-decr))
                ((= 1 r) (zero))
                (else (list (- r 1)))))))))


(define report-error-zero-decr
  (lambda ()
    (eopl:error 'predecessor
                "Zero is not allowed to predecessor")))

; operations have classify
; plus : num * num -> num
(define plus
  (lambda (x y)
    (if (is-zero? x)
        y
        ;(successor (plus (predecessor x) y)))))
        (plus (predecessor x) (successor y)))))

; multi : num * num -> num
(define multi
  (lambda (x y)
    (multi-helper x y (zero))))

; multi : num * num * num -> num
(define multi-helper
  (lambda (x y result)
    (if (is-zero? x)
        result
        (multi-helper (predecessor x) y (plus y result)))))

; fac : num -> num
(define fac
  (lambda (n)
;    (if (is-zero? n)
;        (successor (zero))
;        (multi n (fac (predecessor n)))))) ; control context is grow
    (fac-helper n (successor (zero))))) ; tail call, control context is not grow.

; fac : num * num -> num
(define fac-helper
  (lambda (n result)
    (if (is-zero? n)
        result
        (fac-helper (predecessor n) (multi n result)))))




