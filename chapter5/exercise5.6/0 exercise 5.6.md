Exercise 5.6 [**] Add a list expression to the language, as in exercise 3.10. As
a hint, consider adding two new continuation-builders, one for evaluating the first
element of the list and one for evaluating the rest of the list.

Exercise 3.10 [* *] Add an operation list to the language. This operation should
take any number of arguments, and return an expressed value containing the list of
their values. For example,
let x = 4
in list(x, -(x,1), -(x,3))
should return an expressed value that represents the list (4 3 1).