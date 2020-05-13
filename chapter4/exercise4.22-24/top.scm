(module top (lib "eopl.ss" "eopl")

  ;; top level module.  Loads all required pieces.
  ;; Run the test suite with (run-all).

  (require "drscheme-init.scm")
  (require "data-structures.scm")  ; for expval constructors
  (require "lang.scm")             ; for scan&parse
  (require "interp.scm")           ; for result-of-program

  (provide run )


  ;; run : String -> ExpVal

  (define run
    (lambda (string)
      (result-of-program (scan&parse string))))
  
)




