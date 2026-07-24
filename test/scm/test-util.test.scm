;;; # Unsorted tests
;;;
;;; Tests that have not been sorted yet.

(require (only-in "./test-util"
                  assert-equal
                  compile-repl-form
                  simplify-repl-form
                  test-repl
                  test-macro))

(declare-macro test-macro)

(test-macro
 ;; `test-repl`
 > (describe "test-repl")
 _
 > (it "(> ...)"
       (test-repl
        '(> (+ 1 1)
            2)))
 _
 > (it "(_ > ...)"
       (test-repl
        '(_
          > (+ 1 1)
          2)))
 _
 > (it "(repl > ...)"
       (test-repl
        '(repl
          > (+ 1 1)
          2)))
 _
 > (it "(shell > ...)"
       (test-repl
        '(shell
          > (+ 1 1)
          2)))
 _
 > (it "(roselisp > ...)"
       (test-repl
        '(roselisp
          > (+ 1 1)
          2)))
 _
 > (it "($ roselisp > ...)"
       (test-repl
        '($ roselisp
            ;; Roselisp REPL.
            > (+ 1 1)
            2)))
 _
 > (it "(+ 2 2)"
       (test-repl
        '(roselisp
          > (+ 2 2)
          4)))
 _
 > (it "(list 1 2 3 4)"
       (test-repl
        '(roselisp
          > (list 1 2 3 4)
          '(1 2 3 4))))
 _
 > (it "1 + 1"
       (test-repl
        '(node
          > "1 + 1"
          "2")))
 _
 xit> (it "const n = 1"
          (test-repl
           '(node
             > "const n = 1"
             "undefined"
             > "n + 1"
             "2")))
 _

 ;; `compile-repl-form`
 > (describe "compile-repl-form")
 _
 > (it "(roselisp > (+ 1 1) 2)"
       (compile-repl-form
        '(roselisp
          > (+ 1 1)
          2)
        (js-obj "from" "roselisp"
                "to" "node")))
 '(node
   > "(function () {
  return 1 + 1;
})()"
   "(function () {
  return 2;
})()")
 > (it "(roselisp > (+ 1 1) _)"
       (compile-repl-form
        '(roselisp
          > (+ 1 1)
          _)
        (js-obj "from" "roselisp"
                "to" "node")))
 '(node
   > "(function () {
  return 1 + 1;
})()"
   "_")
 xit> (it "(roselisp > (+ 1 1) 2), plist"
          (compile-repl-form
           '(roselisp
             > (+ 1 1)
             2)
           :from "roselisp"
           :to "node"))
 '(node
   > "(function () {
  return 1 + 1;
})()"
   "(function () {
  return 2;
})()")

 ;; `simplify-repl-form`
 > (describe "simplify-repl-form")
 _
 > (it "(roselisp > (+ 1 1) 2)"
       (simplify-repl-form
        '(roselisp
          > (+ 1 1)
          2
          > (+ 2 2)
          4)))
 '(roselisp
   > (begin
       (+ 1 1)
       (+ 2 2))
   4))
