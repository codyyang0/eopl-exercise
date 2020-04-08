Exercise 1.11 [*] In the last line of subst-in-s-exp, the recursion is on sexp and not a smaller substructure. Why is the recursion guaranteed to halt?</br>

Because S-exp ::= Symbol | S-list, a symbol or s-list, it can be smaller used subst function when it's a s-list.