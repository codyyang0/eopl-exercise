Exercise 5.19 [**] Instead of placing the (lambda () ...) around the body of apply-procedure/k, place it around the body of apply-cont. Modify the contracts to match this change. Does the definition of Bounce need to change? Then replace the procedural representation of Bounce with a data-structure representation, as in exercise 5.18.

1. bounce : ExpVal | () -> Bounce， it's not need to change the contract when placing the (lambda() ...) around the body of apply-cont. Because apply-cont contract = cont * ExpVal -> Bounce, and apply-cont invokes value-of/k and apply-cont and apply-procedure/k which is return a bounce. So place (lambda() ...) around the body of apply-acount get () -> Bounce;

