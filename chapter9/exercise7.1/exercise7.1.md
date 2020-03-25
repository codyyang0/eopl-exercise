1. proc (x) -(x, 3)
> (int -> int)
2. proc (f) proc (x) -((f x), 1)
> ((int -> int) -> (int -> int))
3. proc (x) x
> (t -> t)
4. proc (x) proc (y) (x y)
> ((t -> t) -> (t -> t))
5. proc (x) (x 3)
> ((int -> t) -> t)
6. proc (x) (x x)
> ((t -> t) -> t)
7. proc (x) proc (y) if x then 88 else 99
> (bool -> ())
8. proc (x) proc (y) if x then y else 99
> no-type
9. (proc (p) if p then 88 else 99
    33)
> int
10. (proc (p) if p then 88 else 99
    proc (z) z)
> (t -> t)
11. proc (f)
     proc (g)
      proc (p)
       proc (x) if (p (f x)) then (g 1) else -((f x), 1)
> ((int -> int)
   -> ((int -> int)
      	-> ((int -> bool)
         	 -> (int -> int)))
12. proc (x)
	 proc (p)
	  proc (f)
	   if (p x) then -(x, 1) else (f p)
> (int ->
    ((int -> bool)
      -> ((int -> bool)
          -> int)))
13. proc (f)
     let d = proc (x)
              proc (z) ((f (x x)) z)
     in proc (n) ((f (d d)) n)
> pass now
