Exercise 5.5 [*] Add lists to the language, as in exercise 3.9.

Exercise 3.9 [**] Add list processing operations to the language, including cons,
car, cdr, null? and emptylist. A list should be able to contain any expressed
value, including another list. Give the definitions of the expressed and denoted values of the language, as in section 3.2.2. For example,
let x = 4
in cons(x,
		cons(cons(-(x,1),
				  emptylist),
			 emptylist))
should return an expressed value that represents the list (4 (3)).