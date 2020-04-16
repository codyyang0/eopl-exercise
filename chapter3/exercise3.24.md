Exercise 3.24 [ ] Use the tricks of the program above to write the pair of mutually recursive procedures, odd and even, as in exercise 3.32.

(run "let even = proc (func1) proc (func2) proc (x) if zero?(x) then 1 else (((func1 func2) func1) (- x 1))
  in let odd = proc (func1) proc(func2) proc (x) if zero?(x) then 0 else (((func1 func2) func1) (- x 1))
    in let odd = proc (x) (((odd even) odd) x)
      in let even = proc (x) (((even odd) even) x)
      in (odd 13)")
