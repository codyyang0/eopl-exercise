#lang eopl

;Exercise 2.4 [**] Consider the data type of stacks of values, with an interface consisting of the procedures empty-stack, push, pop, top, and empty-stack?. Write a specification for these operations in the style of the example above. Which operations are constructors and which are observers?

; empty-stack : -> stack
(define empty-stack
  (lambda () '()))


; push : schemeVal * stack -> stack
(define push cons)

; pop : stack -> stack
(define pop cdr)

; top : stack -> schemeVal
(define top car)

; empty-stack
(define empty-stack? null?)
