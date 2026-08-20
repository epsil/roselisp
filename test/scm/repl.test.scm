;;; # REPL
;;;
;;; Various tests of the interactive interface.

(require (only-in "../../src/ts/repl"
                  rep))
(require (only-in "./test-util"
                  assert-equal
                  test-macro))

(declare-macro test-macro)

(test-macro
 > (describe "REPL")
 _
 > (rep "#t")
 "#t"
 > (rep "\"string\"")
 "\"string\""
 > (rep "(+ 1 1)")
 "2"
 > (rep "(+ 1 1) (+ 1 1)")
 "2
2")
