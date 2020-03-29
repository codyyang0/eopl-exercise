Exercise 5.10 [**] Modify the solution to the previous exercise so that the environment is not kept in the continuation.

Exercise 5.9 [**] Modify this interpreter to implement the IMPLICIT-REFS language. As a hint, consider including a new continuation-builder (set-rhs-cont
env var cont).

I let environments be a state factory look like store. Use extend-env! extend-env*! extend-env-rec! to extend the current env.
Then something also changed by this implementation. We need save current env and then find search-sym when we can't find search-sym in current env. Change the env to saved env and find the search-sym. return the ref and set env to save env If we found the search-sym else report error.

Something also changed. Interpretor didn't ship the environment.Then there were some works need to do. Some expression need value-of/k on the new environment, we saved the current environment and value-of and reset the current environment. these code spread in value-of/k, apply-procedure/k, apply-cont. I thinks these way of solutions is not good.

I found y-combinator-1 couldn't through the test when I finish my codes. something was wrong.