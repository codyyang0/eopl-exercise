#lang eopl

;Exercise 2.30 [**] The procedure parse-expression as defined above is fragile: it does
;not detect several possible syntactic errors, such as (a b c), and aborts with
;inappropriate error messages for other expressions, such as (lambda). Modify it so
;that it is robust, accepting any s-exp and issuing an appropriate error message if the
;s-exp does not represent a lambda-calculus expression.

;pass now