Exercise 5.7 [**] Add multideclaration let (exercise 3.16) to this interpreter.


Exercise 3.16 [**] Extend the language so that a let declaration can declare an arbitrary number of variables, using the grammar
Expression ::= let {Identifier = Expression}∗ in Expression
As in Scheme’s let, each of the right-hand sides is evaluated in the current environment, and the body is evaluated with each new variable bound to the value of its
associated right-hand side. For example,
let x = 30
in let x = -(x,1)
y = -(x,2)
in -(x,y)
should evaluate to 1.