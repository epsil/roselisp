;;; # Unsorted tests
;;;
;;; Tests that have not been sorted yet.

(require chai "chai")
(require (only-in "./test-util"
                  assert-equal
                  compile-repl-form
                  simplify-repl-form
                  test-repl))

(describe "test-repl"
  (fn ()
    (describe "roselisp REPL"
      (fn ()
        (it "(> ...)"
            (fn ()
              (test-repl
               '(> (+ 1 1)
                   2))))
        (it "(_ > ...)"
            (fn ()
              (test-repl
               '(_
                 > (+ 1 1)
                 2))))
        (it "(repl > ...)"
            (fn ()
              (test-repl
               '(repl
                 > (+ 1 1)
                 2))))
        (it "(shell > ...)"
            (fn ()
              (test-repl
               '(shell
                 > (+ 1 1)
                 2))))
        (it "(roselisp > ...)"
            (fn ()
              (test-repl
               '(roselisp
                 > (+ 1 1)
                 2))))
        (it "($ roselisp > ...)"
            (fn ()
              (test-repl
               '($ roselisp
                   ;; Roselisp REPL.
                   > (+ 1 1)
                   2))))
        (it "(+ 2 2)"
            (fn ()
              (test-repl
               '(roselisp
                 > (+ 2 2)
                 4))))
        (it "(list 1 2 3 4)"
            (fn ()
              (test-repl
               '(roselisp
                 > (list 1 2 3 4)
                 '(1 2 3 4)))))))
    (describe "node REPL"
      (fn ()
        (it "1 + 1"
            (fn ()
              (test-repl
               '(node
                 > "1 + 1"
                 "2"))))
        (xit "const n = 1"
             (fn ()
               (test-repl
                '(node
                  > "const n = 1"
                  "undefined"
                  > "n + 1"
                  "2"))))))))

(describe "compile-repl-form"
  (fn ()
    (it "(roselisp > (+ 1 1) 2)"
        (fn ()
          (assert-equal
           (compile-repl-form
            '(roselisp
              > (+ 1 1)
              2)
            (js-obj "from" "roselisp"
                    "to" "node"))
           '(node
             > "1 + 1"
             "2"))))
    (it "(roselisp > (+ 1 1) _)"
        (fn ()
          (assert-equal
           (compile-repl-form
            '(roselisp
              > (+ 1 1)
              _)
            (js-obj "from" "roselisp"
                    "to" "node"))
           '(node
             > "1 + 1"
             "_"))))
    (xit "(roselisp > (+ 1 1) 2), plist"
         (fn ()
           (assert-equal
            (compile-repl-form
             '(roselisp
               > (+ 1 1)
               2)
             :from "roselisp"
             :to "node")
            '(node
              > "1 + 1"
              "2"))))))

(describe "simplify-repl-form"
  (fn ()
    (it "(roselisp > (+ 1 1) 2)"
        (fn ()
          (assert-equal
           (simplify-repl-form
            '(roselisp
              > (+ 1 1)
              2
              > (+ 2 2)
              4))
           '(roselisp
             > (begin
                 (+ 1 1)
                 (+ 2 2))
             4))))))
