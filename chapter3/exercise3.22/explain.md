apply-env:
1. If find binding value for search-var, return the value (expval type)
2. If not find binding value for search-var, then find search-var in build-in environments(use (eval search-var))(procedure type).
3. If not find binding report error.

value-of call-exp  (call-exp identifier (list-of expression))
1.if the binding value of identifier is a proc-val named "res", then use (apply-procedure (expval->proc res) args)
2. else use build-in operation build-in observer procedure apply, use (apply res args)
