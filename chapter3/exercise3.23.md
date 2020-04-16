Exercise 3.23 [**] What is the value of the following PROC program?
let makemult = proc (maker)
proc (x)
if zero?(x)
then 0
else -(((maker maker) -(x,1)), -4)
in let times4 = proc (x) ((makemult makemult) x)
in (times4 3)
Use the tricks of this program to write a procedure for factorial in PROC. As a hint,
remember that you can use Currying (exercise 3.20) to define a two-argument procedure times.

base exercise3.22:
let fac = proc (maker) proc (x) if (equal? x 1) then 1 else (* ((maker maker) (- x 1)) x)
in let times = proc(x) ((fac fac) x)
in (times 4)

result : 24
