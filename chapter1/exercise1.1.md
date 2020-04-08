1. {3n + 2 | n ∈ N} </br>
- Top-down: </br>
1. n = 2 or </br>
2. n - 3  ∈ S </br></br>
- Bottom-up: </br>
1. 2 ∈ S, or </br>
2. if n ∈ S, then n + 3 ∈ S </br></br>
- Rules of inference:</br>
2 ∈ S </br>
n ∈ S </br>
------- </br>
n + 3 ∈ S </br>

2. {2n + 3m + 1 | n,m ∈ N}
- Top-down: </br>
1. n = 1 or </br>
2. n - 3 ∈ S or </br>
3. n - 2 ∈ S </br>
- Bottom-up: </br>
1. 1 ∈ S, and
2. if n ∈ S, then n + 3 ∈ S, and
3. if n ∈ S, then n + 2 ∈ S
- Rules of inference:</br>
<pre>1 ∈ S</pre></br>

n ∈ S </br>
------- </br>
n + 3 ∈ S </br></br>

n ∈ S </br>
------- </br>
n + 2 ∈ S </br>

3. {(n, 2n + 1) | n ∈ N}
- Top-down: </br>
A tuple (m, n) is in S if and only if 
1. (m, n) = (0, 1) or </br>
2. (m - 1, n - 2) ∈ S
- Bottom-up: </br>
1. (0, 1) ∈ S and
2. if (m, n) ∈ S then (m + 1, n + 2) ∈ S
- Rules of inference:</br>
<pre> (0, 1) ∈ S </pre></br>
(m, n) ∈ S </br>
-------- </br>
(m + 1, n + 2) ∈ S</br>

4. {(n, n<sup>2</sup>) | n ∈ N}
- Top-down: </br>
A tuple (m, n) is in S if and only if 
1. (m, n) = (0, 0) or </br>
2. (m - 1, n -2m + 1) ∈ S</br>
- Bottom-up: </br>
1. (0, 0) ∈ S and
2. if (m, n) ∈ S then (m + 1, n + 2m + 1)  ∈ S </br>
- Rules of inference:</br>
(m, n) ∈ S </br>
-------- </br>
(m + 1, n + 2m + 1) ∈ S</br>

